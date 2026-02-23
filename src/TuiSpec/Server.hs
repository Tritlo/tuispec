{-# LANGUAGE OverloadedStrings #-}

module TuiSpec.Server (
    ServerOptions (..),
    runServer,
) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (when)
import Data.Aeson (FromJSON (parseJSON), Result (Error, Success), Value (Null, Object), eitherDecodeStrict', encode, fromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Char (isAlphaNum, toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import JSONRPC qualified as RPC
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (hFlush, stdin, stdout)
import System.IO.Error (isEOFError, tryIOError)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (Catch), installHandler, sigHUP)
import TuiSpec.Runner (currentView, dumpView, expectNotVisible, expectSnapshot, expectVisible, killSessionChildrenNow, launch, openSession, press, pressCombo, sendLine, typeText, waitForSelector, waitForText)
import TuiSpec.Types (AmbiguityMode (FailOnAmbiguous, FirstVisibleMatch), App (App), Key (..), Modifier (AltModifier, Control, Shift), Rect (Rect), RunOptions (..), Selector (..), SnapshotName (SnapshotName), Tui (..), WaitOptions (..), defaultRunOptions, defaultWaitOptions)

data ServerOptions = ServerOptions
    { serverArtifactsDir :: FilePath
    , serverTerminalCols :: Int
    , serverTerminalRows :: Int
    , serverTimeoutSeconds :: Int
    , serverAmbiguityMode :: AmbiguityMode
    }
    deriving (Eq, Show)

data ActiveSession = ActiveSession
    { activeTui :: Tui
    }

data ServerState = ServerState
    { stateOptions :: ServerOptions
    , stateActiveSession :: IORef (Maybe ActiveSession)
    }

data DispatchOutcome
    = Continue Value
    | Shutdown Value

data RpcFailure = RpcFailure
    { failureCode :: Int
    , failureMessage :: Text
    , failureData :: Maybe Value
    }

runServer :: ServerOptions -> IO ()
runServer options = do
    sessionRef <- newIORef Nothing
    let state = ServerState{stateOptions = options, stateActiveSession = sessionRef}
    _ <- installHandler sigHUP (Catch (handleSighup state)) Nothing
    loop state
  where
    handleSighup state = do
        killActiveChildrenNow state
        exitImmediately ExitSuccess

    loop state = do
        lineResult <- tryIOError (BS8.hGetLine stdin)
        case lineResult of
            Left ioErr
                | isEOFError ioErr -> pure ()
                | otherwise -> pure ()
            Right line ->
                if BS.null line
                    then loop state
                    else do
                        shouldContinue <- handleLine state line
                        when shouldContinue (loop state)

handleLine :: ServerState -> BS.ByteString -> IO Bool
handleLine state line =
    case eitherDecodeStrict' line :: Either String RPC.JSONRPCRequest of
        Left parseErr -> do
            writeErrorResponse (RPC.RequestId Null) (parseError parseErr)
            pure True
        Right request ->
            case validateRequestVersion request of
                Left err -> do
                    writeErrorResponse (requestId request) err
                    pure True
                Right () -> do
                    outcome <- dispatchRequest state request
                    case outcome of
                        Left err -> do
                            writeErrorResponse (requestId request) err
                            pure True
                        Right (Continue resultValue) -> do
                            writeSuccessResponse (requestId request) resultValue
                            pure True
                        Right (Shutdown resultValue) -> do
                            writeSuccessResponse (requestId request) resultValue
                            pure False

validateRequestVersion :: RPC.JSONRPCRequest -> Either RpcFailure ()
validateRequestVersion request =
    if requestVersion request == RPC.rPC_VERSION
        then Right ()
        else Left (invalidRequest "Expected jsonrpc field to equal \"2.0\"")

dispatchRequest :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchRequest state request =
    case requestMethod request of
        "initialize" -> dispatchInitialize state request
        "launch" -> dispatchLaunch state request
        "sendKey" -> dispatchSendKey state request
        "sendText" -> dispatchSendText state request
        "sendLine" -> dispatchSendLine state request
        "currentView" -> dispatchCurrentView state request
        "dumpView" -> dispatchDumpView state request
        "expectSnapshot" -> dispatchExpectSnapshot state request
        "waitForText" -> dispatchWaitForText state request
        "expectVisible" -> dispatchExpectVisible state request
        "expectNotVisible" -> dispatchExpectNotVisible state request
        "server.ping" -> dispatchPing request
        "server.shutdown" -> dispatchShutdown state request
        unknownMethod ->
            pure $
                Left
                    ( RpcFailure
                        { failureCode = RPC.mETHOD_NOT_FOUND
                        , failureMessage = "Method not found"
                        , failureData = Just (object ["method" .= unknownMethod])
                        }
                    )

dispatchInitialize :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchInitialize state request = do
    existing <- readIORef (stateActiveSession state)
    case existing of
        Just _ ->
            pure (Left sessionAlreadyStartedError)
        Nothing ->
            case decodeParams request of
                Left err -> pure (Left err)
                Right params -> do
                    let sessionName = T.unpack (T.strip (fromMaybeText "session" (startName params)))
                    if null sessionName
                        then pure (Left (invalidParams "session name cannot be empty"))
                        else do
                            case resolveAmbiguityOverride (startAmbiguityMode params) of
                                Left ambiguityErr ->
                                    pure (Left (invalidParams ambiguityErr))
                                Right ambiguityOverride -> do
                                    let runOptions = applyStartParams (stateOptions state) params ambiguityOverride
                                    sessionResult <- try (openSession runOptions sessionName) :: IO (Either SomeException Tui)
                                    case sessionResult of
                                        Left err ->
                                            pure (Left (methodFailed (displayException err)))
                                        Right tui -> do
                                            writeIORef (stateActiveSession state) (Just (ActiveSession tui))
                                            pure $
                                                Right $
                                                    Continue
                                                        ( object
                                                            [ "sessionName" .= sessionName
                                                            , "artifactRoot" .= tuiTestRoot tui
                                                            , "rows" .= terminalRows (tuiOptions tui)
                                                            , "cols" .= terminalCols (tuiOptions tui)
                                                            ]
                                                        )

dispatchLaunch :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchLaunch state request =
    withActiveSession state $ \active ->
        case decodeParams request of
            Left err -> pure (Left err)
            Right params ->
                runMethod $
                    launch (activeTui active) (App (launchCommand params) (launchArgs params))
                        >> pure (object ["ok" .= True])

dispatchSendKey :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchSendKey state request =
    withActiveSession state $ \active ->
        case decodeParams request of
            Left err -> pure (Left err)
            Right params ->
                case parseSendKey (sendKeyValue params) of
                    Left keyErr -> pure (Left (invalidParams keyErr))
                    Right (modifiers, keyValue) ->
                        runMethod $
                            do
                                if null modifiers
                                    then press (activeTui active) keyValue
                                    else pressCombo (activeTui active) modifiers keyValue
                                pure (object ["ok" .= True])

dispatchSendText :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchSendText state request =
    withActiveSession state $ \active ->
        case decodeParams request of
            Left err -> pure (Left err)
            Right params ->
                runMethod $
                    typeText (activeTui active) (sendTextValue params)
                        >> pure (object ["ok" .= True])

dispatchSendLine :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchSendLine state request =
    withActiveSession state $ \active ->
        case decodeParams request of
            Left err -> pure (Left err)
            Right params ->
                runMethod $
                    sendLine (activeTui active) (sendLineValue params)
                        >> pure (object ["ok" .= True])

dispatchCurrentView :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchCurrentView state request =
    withActiveSession state $ \active ->
        case requireNoParams request of
            Left err -> pure (Left err)
            Right () ->
                runMethod $ do
                    textValue <- currentView (activeTui active)
                    let options = tuiOptions (activeTui active)
                    pure
                        ( object
                            [ "text" .= textValue
                            , "rows" .= terminalRows options
                            , "cols" .= terminalCols options
                            ]
                        )

dispatchDumpView :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchDumpView state request =
    withActiveSession state $ \active ->
        case decodeParams request of
            Left err -> pure (Left err)
            Right params ->
                runMethod $ do
                    ansiPath <- dumpView (activeTui active) (SnapshotName (dumpName params))
                    pure
                        ( object
                            [ "snapshotPath" .= ansiPath
                            , "metaPath" .= snapshotMetaPath ansiPath
                            ]
                        )

dispatchExpectSnapshot :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchExpectSnapshot state request =
    withActiveSession state $ \active ->
        case decodeParams request of
            Left err -> pure (Left err)
            Right params ->
                runMethod $ do
                    let snapshotText = expectSnapshotNameValue params
                    let snapshotStem = safeSnapshotStem (T.unpack snapshotText)
                    let tui = activeTui active
                    let actualPath = tuiTestRoot tui </> "snapshots" </> (snapshotStem <> ".ansi.txt")
                    let baselinePath = tuiSnapshotRoot tui </> (snapshotStem <> ".ansi.txt")
                    expectSnapshot tui (SnapshotName snapshotText)
                    baselineExists <- doesFileExist baselinePath
                    pure
                        ( object
                            [ "ok" .= True
                            , "actualPath" .= actualPath
                            , "baselinePath" .= baselinePath
                            , "baselineExists" .= baselineExists
                            ]
                        )

dispatchWaitForText :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchWaitForText state request =
    withActiveSession state $ \active ->
        case decodeParams request of
            Left err -> pure (Left err)
            Right params ->
                runMethod $ do
                    let tui = activeTui active
                    case (waitTimeoutMs params, waitPollIntervalMs params) of
                        (Nothing, Nothing) ->
                            waitForText tui (waitSelector params)
                        _ ->
                            waitForSelector
                                tui
                                defaultWaitOptions
                                    { timeoutMs = maybe (timeoutMs defaultWaitOptions) id (waitTimeoutMs params)
                                    , pollIntervalMs = maybe (pollIntervalMs defaultWaitOptions) id (waitPollIntervalMs params)
                                    }
                                (waitSelector params)
                    pure (object ["ok" .= True])

dispatchExpectVisible :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchExpectVisible state request =
    withActiveSession state $ \active ->
        case decodeParams request of
            Left err -> pure (Left err)
            Right params ->
                runMethod $
                    expectVisible (activeTui active) (selectorValue params)
                        >> pure (object ["ok" .= True])

dispatchExpectNotVisible :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchExpectNotVisible state request =
    withActiveSession state $ \active ->
        case decodeParams request of
            Left err -> pure (Left err)
            Right params ->
                runMethod $
                    expectNotVisible (activeTui active) (selectorValue params)
                        >> pure (object ["ok" .= True])

dispatchPing :: RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchPing request =
    case requireNoParams request of
        Left err -> pure (Left err)
        Right () ->
            pure
                ( Right
                    ( Continue
                        ( object
                            [ "pong" .= True
                            , "version" .= ("0.1.0.0" :: String)
                            ]
                        )
                    )
                )

dispatchShutdown :: ServerState -> RPC.JSONRPCRequest -> IO (Either RpcFailure DispatchOutcome)
dispatchShutdown state request =
    case requireNoParams request of
        Left err -> pure (Left err)
        Right () -> do
            killActiveChildrenNow state
            pure
                ( Right
                    ( Shutdown
                        ( object
                            [ "shuttingDown" .= True
                            ]
                        )
                    )
                )

withActiveSession :: ServerState -> (ActiveSession -> IO (Either RpcFailure DispatchOutcome)) -> IO (Either RpcFailure DispatchOutcome)
withActiveSession state action = do
    maybeActive <- readIORef (stateActiveSession state)
    case maybeActive of
        Nothing -> pure (Left noActiveSessionError)
        Just active -> action active

killActiveChildrenNow :: ServerState -> IO ()
killActiveChildrenNow state = do
    maybeActive <- readIORef (stateActiveSession state)
    case maybeActive of
        Nothing -> pure ()
        Just active -> killSessionChildrenNow (activeTui active)

runMethod :: IO Value -> IO (Either RpcFailure DispatchOutcome)
runMethod action = do
    result <- try action :: IO (Either SomeException Value)
    pure $
        case result of
            Left err -> Left (methodFailed (displayException err))
            Right value -> Right (Continue value)

writeSuccessResponse :: RPC.RequestId -> Value -> IO ()
writeSuccessResponse reqId resultValue =
    writeJsonLine
        ( encode
            (RPC.JSONRPCResponse RPC.rPC_VERSION reqId resultValue)
        )

writeErrorResponse :: RPC.RequestId -> RpcFailure -> IO ()
writeErrorResponse reqId failure =
    writeJsonLine
        ( encode
            ( RPC.JSONRPCError
                RPC.rPC_VERSION
                reqId
                (RPC.JSONRPCErrorInfo (failureCode failure) (failureMessage failure) (failureData failure))
            )
        )

writeJsonLine :: BL8.ByteString -> IO ()
writeJsonLine bytes = do
    BL8.hPutStr stdout bytes
    BL8.hPutStr stdout "\n"
    hFlush stdout

decodeParams :: (FromJSON a) => RPC.JSONRPCRequest -> Either RpcFailure a
decodeParams request =
    case fromJSON (requestParams request) of
        Error err -> Left (invalidParams err)
        Success value -> Right value

requireNoParams :: RPC.JSONRPCRequest -> Either RpcFailure ()
requireNoParams request =
    case requestParams request of
        Null -> Right ()
        Object keyMap
            | KM.null keyMap -> Right ()
        _ -> Left (invalidParams "expected params to be null or empty object")

invalidRequest :: String -> RpcFailure
invalidRequest details =
    RpcFailure
        { failureCode = RPC.iNVALID_REQUEST
        , failureMessage = "Invalid request"
        , failureData = Just (object ["details" .= details])
        }

invalidParams :: String -> RpcFailure
invalidParams details =
    RpcFailure
        { failureCode = RPC.iNVALID_PARAMS
        , failureMessage = "Invalid params"
        , failureData = Just (object ["details" .= details])
        }

parseError :: String -> RpcFailure
parseError details =
    RpcFailure
        { failureCode = RPC.pARSE_ERROR
        , failureMessage = "Parse error"
        , failureData = Just (object ["details" .= details])
        }

methodFailed :: String -> RpcFailure
methodFailed details =
    RpcFailure
        { failureCode = -32004
        , failureMessage = "Method failed"
        , failureData = Just (object ["details" .= details])
        }

noActiveSessionError :: RpcFailure
noActiveSessionError =
    RpcFailure
        { failureCode = -32001
        , failureMessage = "No active session"
        , failureData = Nothing
        }

sessionAlreadyStartedError :: RpcFailure
sessionAlreadyStartedError =
    RpcFailure
        { failureCode = -32002
        , failureMessage = "Session already started"
        , failureData = Nothing
        }

data StartParams = StartParams
    { startName :: Maybe Text
    , startTimeoutSeconds :: Maybe Int
    , startTerminalCols :: Maybe Int
    , startTerminalRows :: Maybe Int
    , startAmbiguityMode :: Maybe Text
    , startSnapshotTheme :: Maybe Text
    , startUpdateSnapshots :: Maybe Bool
    }

instance FromJSON StartParams where
    parseJSON value =
        case value of
            Null -> pure defaultStartParams
            _ ->
                withObject "StartParams" parseObject value
      where
        parseObject o =
            StartParams
                <$> o .:? "name"
                <*> o .:? "timeoutSeconds"
                <*> o .:? "terminalCols"
                <*> o .:? "terminalRows"
                <*> o .:? "ambiguityMode"
                <*> o .:? "snapshotTheme"
                <*> o .:? "updateSnapshots"

defaultStartParams :: StartParams
defaultStartParams =
    StartParams
        { startName = Nothing
        , startTimeoutSeconds = Nothing
        , startTerminalCols = Nothing
        , startTerminalRows = Nothing
        , startAmbiguityMode = Nothing
        , startSnapshotTheme = Nothing
        , startUpdateSnapshots = Nothing
        }

data LaunchParams = LaunchParams
    { launchCommand :: FilePath
    , launchArgs :: [String]
    }

instance FromJSON LaunchParams where
    parseJSON =
        withObject "LaunchParams" $ \o ->
            LaunchParams
                <$> o .: "command"
                <*> o .:? "args" AesonTypes..!= []

data SendKeyParams = SendKeyParams
    { sendKeyValue :: Text
    }

instance FromJSON SendKeyParams where
    parseJSON =
        withObject "SendKeyParams" $ \o ->
            SendKeyParams <$> o .: "key"

data SendTextParams = SendTextParams
    { sendTextValue :: Text
    }

instance FromJSON SendTextParams where
    parseJSON =
        withObject "SendTextParams" $ \o ->
            SendTextParams <$> o .: "text"

data SendLineParams = SendLineParams
    { sendLineValue :: Text
    }

instance FromJSON SendLineParams where
    parseJSON =
        withObject "SendLineParams" $ \o ->
            SendLineParams <$> o .: "text"

data DumpViewParams = DumpViewParams
    { dumpName :: Text
    }

instance FromJSON DumpViewParams where
    parseJSON =
        withObject "DumpViewParams" $ \o ->
            DumpViewParams <$> o .: "name"

data ExpectSnapshotParams = ExpectSnapshotParams
    { expectSnapshotNameValue :: Text
    }

instance FromJSON ExpectSnapshotParams where
    parseJSON =
        withObject "ExpectSnapshotParams" $ \o ->
            ExpectSnapshotParams <$> o .: "name"

data SelectorParams = SelectorParams
    { selectorValue :: Selector
    }

instance FromJSON SelectorParams where
    parseJSON =
        withObject "SelectorParams" $ \o ->
            SelectorParams <$> (o .: "selector" >>= parseSelector)

data WaitForTextParams = WaitForTextParams
    { waitSelector :: Selector
    , waitTimeoutMs :: Maybe Int
    , waitPollIntervalMs :: Maybe Int
    }

instance FromJSON WaitForTextParams where
    parseJSON =
        withObject "WaitForTextParams" $ \o ->
            WaitForTextParams
                <$> (o .: "selector" >>= parseSelector)
                <*> o .:? "timeoutMs"
                <*> o .:? "pollIntervalMs"

parseSelector :: Value -> AesonTypes.Parser Selector
parseSelector =
    withObject "Selector" $ \o -> do
        selectorType <- (T.toLower <$> (o .: "type" :: AesonTypes.Parser Text))
        case selectorType of
            "exact" -> Exact <$> o .: "text"
            "regex" -> Regex <$> o .: "pattern"
            "at" -> At <$> o .: "col" <*> o .: "row"
            "within" -> do
                rectValue <- o .: "rect" >>= parseRect
                nested <- o .: "selector" >>= parseSelector
                pure (Within rectValue nested)
            "nth" -> Nth <$> o .: "index" <*> (o .: "selector" >>= parseSelector)
            _ -> fail ("unknown selector type: " <> T.unpack selectorType)

parseRect :: Value -> AesonTypes.Parser Rect
parseRect =
    withObject "Rect" $ \o ->
        Rect
            <$> o .: "col"
            <*> o .: "row"
            <*> o .: "width"
            <*> o .: "height"

requestId :: RPC.JSONRPCRequest -> RPC.RequestId
requestId (RPC.JSONRPCRequest _ reqId _ _) = reqId

requestVersion :: RPC.JSONRPCRequest -> Text
requestVersion (RPC.JSONRPCRequest version _ _ _) = version

requestMethod :: RPC.JSONRPCRequest -> Text
requestMethod (RPC.JSONRPCRequest _ _ methodName _) = methodName

requestParams :: RPC.JSONRPCRequest -> Value
requestParams (RPC.JSONRPCRequest _ _ _ paramsValue) = paramsValue

applyStartParams :: ServerOptions -> StartParams -> Maybe AmbiguityMode -> RunOptions
applyStartParams options params ambiguityOverride =
    defaultRunOptions
        { timeoutSeconds = maybe (serverTimeoutSeconds options) id (startTimeoutSeconds params)
        , terminalCols = maybe (serverTerminalCols options) id (startTerminalCols params)
        , terminalRows = maybe (serverTerminalRows options) id (startTerminalRows params)
        , artifactsDir = serverArtifactsDir options
        , ambiguityMode = maybe (serverAmbiguityMode options) id ambiguityOverride
        , updateSnapshots = maybe False id (startUpdateSnapshots params)
        , snapshotTheme = maybe "auto" T.unpack (startSnapshotTheme params)
        }

resolveAmbiguityOverride :: Maybe Text -> Either String (Maybe AmbiguityMode)
resolveAmbiguityOverride maybeRaw =
    case maybeRaw of
        Nothing -> Right Nothing
        Just raw ->
            case parseAmbiguityMode raw of
                Just mode -> Right (Just mode)
                Nothing ->
                    Left "ambiguityMode must be one of: fail, first, first-visible"

parseAmbiguityMode :: Text -> Maybe AmbiguityMode
parseAmbiguityMode raw =
    case map toLower (T.unpack (T.strip raw)) of
        "fail" -> Just FailOnAmbiguous
        "first" -> Just FirstVisibleMatch
        "first-visible" -> Just FirstVisibleMatch
        _ -> Nothing

parseSendKey :: Text -> Either String ([Modifier], Key)
parseSendKey rawKey =
    case map T.strip (T.splitOn "+" (T.strip rawKey)) of
        [baseKey] ->
            (,) [] <$> parseBaseKey baseKey
        [modifierText, keyText] ->
            do
                modifier <- parseModifier modifierText
                keyValue <- parseModifiedKey keyText
                pure ([modifier], keyValue)
        _ ->
            Left "key must be a simple key or modifier combo like Ctrl+C"
  where
    parseModifier textValue =
        case map toLower (T.unpack textValue) of
            "ctrl" -> Right Control
            "control" -> Right Control
            "alt" -> Right AltModifier
            "shift" -> Right Shift
            _ -> Left ("unknown modifier: " <> T.unpack textValue)

    parseModifiedKey textValue =
        case T.unpack textValue of
            [charValue] -> Right (CharKey charValue)
            _ -> Left "modified keys must use a single character (for example Ctrl+C)"

parseBaseKey :: Text -> Either String Key
parseBaseKey keyText =
    let lowered = map toLower (T.unpack (T.strip keyText))
     in case lowered of
            "enter" -> Right Enter
            "esc" -> Right Esc
            "escape" -> Right Esc
            "tab" -> Right Tab
            "backspace" -> Right Backspace
            "arrowup" -> Right ArrowUp
            "arrowdown" -> Right ArrowDown
            "arrowleft" -> Right ArrowLeft
            "arrowright" -> Right ArrowRight
            "space" -> Right (CharKey ' ')
            _ ->
                case parseFunctionKey lowered of
                    Just functionNumber -> Right (FunctionKey functionNumber)
                    Nothing ->
                        case T.unpack (T.strip keyText) of
                            [charValue] -> Right (CharKey charValue)
                            _ -> Left ("unknown key: " <> T.unpack keyText)

parseFunctionKey :: String -> Maybe Int
parseFunctionKey lowered =
    case lowered of
        'f' : digits
            | all isDigitAscii digits ->
                case reads digits of
                    [(value, "")]
                        | value >= 1 && value <= 12 -> Just value
                    _ -> Nothing
        _ -> Nothing
  where
    isDigitAscii c = c >= '0' && c <= '9'

snapshotMetaPath :: FilePath -> FilePath
snapshotMetaPath ansiPath =
    if ".ansi.txt" `isSuffixOf` ansiPath
        then take (length ansiPath - length (".ansi.txt" :: String)) ansiPath <> ".meta.json"
        else ansiPath <> ".meta.json"

safeSnapshotStem :: String -> String
safeSnapshotStem input =
    let lowered = map toLower input
        normalized = map normalize lowered
        compact = collapseDashes normalized
        trimmed = trimDashes compact
     in if null trimmed then "snapshot" else trimmed
  where
    normalize c
        | isAlphaNum c = c
        | otherwise = '-'

    collapseDashes [] = []
    collapseDashes ('-' : '-' : rest) = collapseDashes ('-' : rest)
    collapseDashes (c : rest) = c : collapseDashes rest

    trimDashes = reverse . dropWhile (== '-') . reverse . dropWhile (== '-')

fromMaybeText :: Text -> Maybe Text -> Text
fromMaybeText fallback maybeValue =
    case maybeValue of
        Just value -> value
        Nothing -> fallback
