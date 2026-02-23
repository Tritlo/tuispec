{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TuiSpec.Server
Description : JSON-RPC 2.0 server for interactive TUI orchestration.

Runs a newline-delimited JSON-RPC server on stdin\/stdout, allowing
external tools to drive TUI sessions programmatically.
-}
module TuiSpec.Server (
    ServerOptions (..),
    runServer,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, finally, throwIO, try)
import Control.Monad (foldM, when)
import Data.Aeson (FromJSON (parseJSON), Result (Error, Success), Value (Null, Object), eitherDecode, eitherDecodeStrict', encode, fromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Char (isAlphaNum, toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import JSONRPC qualified as RPC
import System.Directory (canonicalizePath, doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (hFlush, stdin, stdout)
import System.IO.Error (isEOFError, tryIOError)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (Catch), installHandler, sigHUP)
import TuiSpec.Render (renderAnsiSnapshotFileWithFont)
import TuiSpec.Replay (RecordingDirection (DirectionNotification, DirectionRequest, DirectionResponse), RecordingHandle, ReplaySpeed (ReplayAsFastAsPossible, ReplayRealTime), appendRecordingEvent, closeRecording, openRecording, readRecordingEvents, replayRecordedRequests)
import TuiSpec.Runner (currentView, defaultWaitOptionsFor, dumpView, expectNotVisible, expectSnapshot, expectVisible, killSessionChildrenNow, launch, openSession, press, pressCombo, renderAnsiViewportText, sendLine, serializeAnsiSnapshot, typeText, waitForSelectorWithAmbiguity)
import TuiSpec.Types (AmbiguityMode (FailOnAmbiguous, FirstVisibleMatch, LastVisibleMatch), App (..), Key (..), Modifier (Alt, Control, Shift), Rect (Rect), RunOptions (..), Selector (..), SnapshotName (SnapshotName), Tui (..), WaitOptions (..), defaultRunOptions)

-- | Configuration for the JSON-RPC server.
data ServerOptions = ServerOptions
    { serverArtifactsDir :: FilePath
    -- ^ Base directory for session artifacts.
    , serverTerminalCols :: Int
    -- ^ Default terminal columns for launched sessions.
    , serverTerminalRows :: Int
    -- ^ Default terminal rows for launched sessions.
    , serverTimeoutSeconds :: Int
    -- ^ Default timeout for wait operations.
    , serverAmbiguityMode :: AmbiguityMode
    -- ^ Default ambiguity mode for selector assertions.
    }
    deriving (Eq, Show)

data ActiveSession = ActiveSession
    { activeTui :: Tui
    }

data RecordingSession = RecordingSession
    { activeRecordingPath :: FilePath
    , activeRecordingHandle :: RecordingHandle
    }

data ViewSubscription = ViewSubscription
    { subscriptionDebounceMs :: Int
    , subscriptionIncludeText :: Bool
    , subscriptionLastSentMicros :: Maybe Int64
    , subscriptionLastView :: Maybe Text
    }

data ServerState = ServerState
    { stateOptions :: ServerOptions
    , stateActiveSession :: IORef (Maybe ActiveSession)
    , stateRecording :: IORef (Maybe RecordingSession)
    , stateViewSubscription :: IORef (Maybe ViewSubscription)
    }

data DispatchOutcome
    = Continue Value
    | Shutdown Value

data RpcFailure = RpcFailure
    { failureCode :: Int
    , failureMessage :: Text
    , failureData :: Maybe Value
    }

-- | Run the JSON-RPC server, reading requests from stdin and writing responses to stdout.
runServer :: ServerOptions -> IO ()
runServer options = do
    sessionRef <- newIORef Nothing
    recordingRef <- newIORef Nothing
    subscriptionRef <- newIORef Nothing
    let state =
            ServerState
                { stateOptions = options
                , stateActiveSession = sessionRef
                , stateRecording = recordingRef
                , stateViewSubscription = subscriptionRef
                }
    _ <- installHandler sigHUP (Catch (handleSighup state)) Nothing
    loop state `finally` shutdownServer state
  where
    handleSighup state = do
        killActiveChildrenNow state
        closeActiveRecording state
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

shutdownServer :: ServerState -> IO ()
shutdownServer state = do
    killActiveChildrenNow state
    closeActiveRecording state

handleLine :: ServerState -> BS.ByteString -> IO Bool
handleLine state line = do
    recordIncomingRequestLine state line
    case eitherDecodeStrict' line :: Either String RPC.JSONRPCRequest of
        Left parseErr -> do
            writeErrorResponse state (RPC.RequestId Null) (parseError parseErr)
        Right request ->
            case validateRequestVersion request of
                Left err -> do
                    writeErrorResponse state (requestId request) err
                Right () -> do
                    outcome <- dispatchMethod state (requestMethod request) (requestParams request)
                    case outcome of
                        Left err ->
                            writeErrorResponse state (requestId request) err
                        Right (Continue resultValue) ->
                            writeSuccessResponse state (requestId request) resultValue
                        Right (Shutdown resultValue) -> do
                            _ <- writeSuccessResponse state (requestId request) resultValue
                            pure False

validateRequestVersion :: RPC.JSONRPCRequest -> Either RpcFailure ()
validateRequestVersion request =
    if requestVersion request == RPC.rPC_VERSION
        then Right ()
        else Left (invalidRequest "Expected jsonrpc field to equal \"2.0\"")

dispatchMethod :: ServerState -> Text -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchMethod state methodName paramsValue =
    case methodName of
        "initialize" -> dispatchInitialize state paramsValue
        "launch" -> dispatchLaunch state paramsValue
        "sendKey" -> dispatchSendKey state paramsValue
        "sendText" -> dispatchSendText state paramsValue
        "sendLine" -> dispatchSendLine state paramsValue
        "currentView" -> dispatchCurrentView state paramsValue
        "dumpView" -> dispatchDumpView state paramsValue
        "renderView" -> dispatchRenderView state paramsValue
        "expectSnapshot" -> dispatchExpectSnapshot state paramsValue
        "waitForText" -> dispatchWaitForText state paramsValue
        "waitUntil" -> dispatchWaitUntil state paramsValue
        "diffView" -> dispatchDiffView state paramsValue
        "expectVisible" -> dispatchExpectVisible state paramsValue
        "expectNotVisible" -> dispatchExpectNotVisible state paramsValue
        "viewSubscribe" -> dispatchViewSubscribe state paramsValue
        "viewUnsubscribe" -> dispatchViewUnsubscribe state paramsValue
        "batch" -> dispatchBatch state paramsValue
        "recording.start" -> dispatchRecordingStart state paramsValue
        "recording.stop" -> dispatchRecordingStop state paramsValue
        "recording.status" -> dispatchRecordingStatus state paramsValue
        "replay" -> dispatchReplay state paramsValue
        "server.ping" -> dispatchPing paramsValue
        "server.shutdown" -> dispatchShutdown state paramsValue
        unknownMethod ->
            pure $
                Left
                    ( RpcFailure
                        { failureCode = RPC.mETHOD_NOT_FOUND
                        , failureMessage = "Method not found"
                        , failureData = Just (object ["method" .= unknownMethod])
                        }
                    )

dispatchInitialize :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchInitialize state paramsValue = do
    existing <- readIORef (stateActiveSession state)
    case existing of
        Just _ ->
            pure (Left sessionAlreadyStartedError)
        Nothing ->
            case decodeParamsValue paramsValue of
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

dispatchLaunch :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchLaunch state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                runMethod $ do
                    let tui = activeTui active
                    launch
                        tui
                        App
                            { command = launchCommand params
                            , args = launchArgs params
                            , env = launchEnv params
                            , cwd = launchCwd params
                            }
                    case launchReadySelector params of
                        Nothing -> pure ()
                        Just selector -> do
                            let defaults = defaultWaitOptionsFor tui
                            let waitOptions = mergeWaitOptions defaults (launchReadyTimeoutMs params) (launchReadyPollIntervalMs params)
                            waitForSelectorWithAmbiguity tui waitOptions Nothing selector
                    emitViewChangedNotification state tui
                    pure (object ["ok" .= True])

dispatchSendKey :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchSendKey state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                case parseSendKey (sendKeyValue params) of
                    Left keyErr -> pure (Left (invalidParams keyErr))
                    Right (modifiers, keyValue) ->
                        runMethod $ do
                            let tui = activeTui active
                            if null modifiers
                                then press tui keyValue
                                else pressCombo tui modifiers keyValue
                            emitViewChangedNotification state tui
                            pure (object ["ok" .= True])

dispatchSendText :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchSendText state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                runMethod $ do
                    let tui = activeTui active
                    typeText tui (sendTextValue params)
                    emitViewChangedNotification state tui
                    pure (object ["ok" .= True])

dispatchSendLine :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchSendLine state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                case resolveAmbiguityOverride (sendLineAmbiguityMode params) of
                    Left ambiguityErr -> pure (Left (invalidParams ambiguityErr))
                    Right ambiguityOverride ->
                        runMethod $ do
                            let tui = activeTui active
                            sendLine tui (sendLineValue params)
                            case sendLineExpectAfter params of
                                Nothing -> pure ()
                                Just selector -> do
                                    let defaults = defaultWaitOptionsFor tui
                                    let waitOptions = mergeWaitOptions defaults (sendLineTimeoutMs params) (sendLinePollIntervalMs params)
                                    waitForSelectorWithAmbiguity tui waitOptions ambiguityOverride selector
                            emitViewChangedNotification state tui
                            pure (object ["ok" .= True])

dispatchCurrentView :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchCurrentView state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                let options = tuiOptions (activeTui active)
                    totalRows = terminalRows options
                    totalCols = terminalCols options
                 in case resolveCurrentViewFilter params totalRows totalCols of
                        Left filterErr -> pure (Left (invalidParams filterErr))
                        Right filterValue ->
                            runMethod $ do
                                textValue <- currentView (activeTui active)
                                let (filteredText, outRows, outCols) = applyCurrentViewFilter filterValue totalRows totalCols textValue
                                pure
                                    ( object
                                        [ "text" .= filteredText
                                        , "rows" .= outRows
                                        , "cols" .= outCols
                                        ]
                                    )

dispatchDumpView :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchDumpView state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                runMethod $ do
                    let tui = activeTui active
                    ansiPathRaw <- dumpView tui (SnapshotName (dumpName params))
                    ansiPath <- canonicalizeExistingPath ansiPathRaw
                    metaPath <- canonicalizeExistingPath (snapshotMetaPath ansiPath)
                    artifactRoot <- canonicalizePath (tuiTestRoot tui)
                    maybePngPath <-
                        case dumpFormat params of
                            DumpAnsi -> pure Nothing
                            DumpPng -> Just <$> renderSnapshotFromDump params ansiPath
                            DumpBoth -> Just <$> renderSnapshotFromDump params ansiPath
                    pure
                        ( object
                            ( [ "snapshotPath" .= ansiPath
                              , "metaPath" .= metaPath
                              , "artifactRoot" .= artifactRoot
                              ]
                                <> maybe [] (\pngPath -> ["pngPath" .= pngPath]) maybePngPath
                            )
                        )

dispatchRenderView :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchRenderView state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                runMethod $ do
                    let tui = activeTui active
                    ansiPathRaw <- dumpView tui (SnapshotName (renderViewName params))
                    ansiPath <- canonicalizeExistingPath ansiPathRaw
                    metaPath <- canonicalizeExistingPath (snapshotMetaPath ansiPath)
                    artifactRoot <- canonicalizePath (tuiTestRoot tui)
                    pngPath <- renderSnapshotFromRenderView params ansiPath
                    pure
                        ( object
                            [ "snapshotPath" .= ansiPath
                            , "metaPath" .= metaPath
                            , "pngPath" .= pngPath
                            , "artifactRoot" .= artifactRoot
                            ]
                        )

dispatchExpectSnapshot :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchExpectSnapshot state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
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

dispatchWaitForText :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchWaitForText state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                case resolveAmbiguityOverride (waitAmbiguityMode params) of
                    Left ambiguityErr -> pure (Left (invalidParams ambiguityErr))
                    Right ambiguityOverride ->
                        runMethod $ do
                            let tui = activeTui active
                            let defaults = defaultWaitOptionsFor tui
                            let mergedWaitOptions = mergeWaitOptions defaults (waitTimeoutMs params) (waitPollIntervalMs params)
                            waitForSelectorWithAmbiguity tui mergedWaitOptions ambiguityOverride (waitSelector params)
                            pure (object ["ok" .= True])

dispatchWaitUntil :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchWaitUntil state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                runMethod $ do
                    let tui = activeTui active
                    let defaults = defaultWaitOptionsFor tui
                    let mergedWaitOptions = mergeWaitOptions defaults (waitUntilTimeoutMs params) (waitUntilPollIntervalMs params)
                    waitUntilPattern tui mergedWaitOptions (waitUntilPatternValue params)
                    pure (object ["ok" .= True])

dispatchDiffView :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchDiffView state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                runMethod $ do
                    let options = tuiOptions (activeTui active)
                    diffResult <- computeSnapshotDiff options params
                    pure
                        ( object
                            [ "changed" .= diffChanged diffResult
                            , "changedLines" .= diffChangedLines diffResult
                            , "summary" .= diffSummary diffResult
                            ]
                        )

dispatchExpectVisible :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchExpectVisible state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                runMethod $
                    expectVisible (activeTui active) (selectorValue params)
                        >> pure (object ["ok" .= True])

dispatchExpectNotVisible :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchExpectNotVisible state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                runMethod $
                    expectNotVisible (activeTui active) (selectorValue params)
                        >> pure (object ["ok" .= True])

dispatchViewSubscribe :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchViewSubscribe state paramsValue =
    withActiveSession state $ \active ->
        case decodeParamsValue paramsValue of
            Left err -> pure (Left err)
            Right params ->
                if subscribeDebounceMs params < 0
                    then pure (Left (invalidParams "debounceMs must be >= 0"))
                    else runMethod $ do
                        let subscription =
                                ViewSubscription
                                    { subscriptionDebounceMs = subscribeDebounceMs params
                                    , subscriptionIncludeText = subscribeIncludeText params
                                    , subscriptionLastSentMicros = Nothing
                                    , subscriptionLastView = Nothing
                                    }
                        writeIORef (stateViewSubscription state) (Just subscription)
                        emitViewChangedNotification state (activeTui active)
                        pure
                            ( object
                                [ "ok" .= True
                                , "debounceMs" .= subscribeDebounceMs params
                                , "includeText" .= subscribeIncludeText params
                                ]
                            )

dispatchViewUnsubscribe :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchViewUnsubscribe state paramsValue =
    case requireNoParamsValue paramsValue of
        Left err -> pure (Left err)
        Right () ->
            runMethod $ do
                writeIORef (stateViewSubscription state) Nothing
                pure (object ["ok" .= True])

dispatchBatch :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchBatch state paramsValue =
    case decodeParamsValue paramsValue of
        Left err -> pure (Left err)
        Right params ->
            runMethod $ do
                (completed, results, maybeFailure) <- foldM runBatchStep (0 :: Int, [], Nothing) (batchSteps params)
                case maybeFailure of
                    Nothing ->
                        pure
                            ( object
                                [ "ok" .= True
                                , "completed" .= completed
                                , "results" .= reverse results
                                ]
                            )
                    Just (stepIndex, failureValue) ->
                        pure
                            ( object
                                [ "ok" .= False
                                , "completed" .= completed
                                , "results" .= reverse results
                                , "errorStep" .= stepIndex
                                , "error" .= failureValue
                                ]
                            )
  where
    runBatchStep (completed, results, Just existingFailure) _ =
        pure (completed, results, Just existingFailure)
    runBatchStep (completed, results, Nothing) stepValue = do
        outcome <- dispatchMethod state (batchStepMethod stepValue) (batchStepParams stepValue)
        case outcome of
            Left err ->
                pure (completed, results, Just (completed + 1, rpcFailureToValue err))
            Right (Shutdown _) ->
                pure (completed, results, Just (completed + 1, object ["code" .= (-32020 :: Int), "message" .= ("batch step cannot call server.shutdown" :: Text)]))
            Right (Continue value) ->
                pure (completed + 1, value : results, Nothing)

dispatchRecordingStart :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchRecordingStart state paramsValue =
    case decodeParamsValue paramsValue of
        Left err -> pure (Left err)
        Right params ->
            runMethod $ do
                closeActiveRecording state
                handle <- openRecording (recordingStartPath params)
                canonicalPath <- canonicalizePath (recordingStartPath params)
                writeIORef
                    (stateRecording state)
                    (Just (RecordingSession canonicalPath handle))
                pure
                    ( object
                        [ "ok" .= True
                        , "path" .= canonicalPath
                        ]
                    )

dispatchRecordingStop :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchRecordingStop state paramsValue =
    case requireNoParamsValue paramsValue of
        Left err -> pure (Left err)
        Right () ->
            runMethod $ do
                closeActiveRecording state
                pure (object ["ok" .= True])

dispatchRecordingStatus :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchRecordingStatus state paramsValue =
    case requireNoParamsValue paramsValue of
        Left err -> pure (Left err)
        Right () ->
            runMethod $ do
                active <- readIORef (stateRecording state)
                pure
                    ( object
                        [ "active" .= isJust active
                        , "path" .= fmap activeRecordingPath active
                        ]
                    )

dispatchReplay :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchReplay state paramsValue =
    case decodeParamsValue paramsValue of
        Left err -> pure (Left err)
        Right params ->
            runMethod $ do
                events <- readRecordingEvents (replayPath params)
                replayed <-
                    replayRecordedRequests
                        (replaySpeed params)
                        events
                        (replayRecordedRequestLine state)
                pure
                    ( object
                        [ "ok" .= True
                        , "replayedRequests" .= replayed
                        , "path" .= replayPath params
                        , "speed" .= renderReplaySpeed (replaySpeed params)
                        ]
                    )

dispatchPing :: Value -> IO (Either RpcFailure DispatchOutcome)
dispatchPing paramsValue =
    case requireNoParamsValue paramsValue of
        Left err -> pure (Left err)
        Right () ->
            pure
                ( Right
                    ( Continue
                        ( object
                            [ "pong" .= True
                            , "version" .= ("0.2.0.0" :: String)
                            ]
                        )
                    )
                )

dispatchShutdown :: ServerState -> Value -> IO (Either RpcFailure DispatchOutcome)
dispatchShutdown state paramsValue =
    case requireNoParamsValue paramsValue of
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

closeActiveRecording :: ServerState -> IO ()
closeActiveRecording state = do
    maybeRecording <- readIORef (stateRecording state)
    case maybeRecording of
        Nothing -> pure ()
        Just recording -> do
            closeRecording (activeRecordingHandle recording)
            writeIORef (stateRecording state) Nothing

runMethod :: IO Value -> IO (Either RpcFailure DispatchOutcome)
runMethod action = do
    result <- try action :: IO (Either SomeException Value)
    pure $
        case result of
            Left err -> Left (methodFailed (displayException err))
            Right value -> Right (Continue value)

writeSuccessResponse :: ServerState -> RPC.RequestId -> Value -> IO Bool
writeSuccessResponse state reqId resultValue =
    writeJsonLine
        state
        DirectionResponse
        ( encode
            (RPC.JSONRPCResponse RPC.rPC_VERSION reqId resultValue)
        )

writeErrorResponse :: ServerState -> RPC.RequestId -> RpcFailure -> IO Bool
writeErrorResponse state reqId failure =
    writeJsonLine
        state
        DirectionResponse
        ( encode
            ( RPC.JSONRPCError
                RPC.rPC_VERSION
                reqId
                (RPC.JSONRPCErrorInfo (failureCode failure) (failureMessage failure) (failureData failure))
            )
        )

writeNotification :: ServerState -> Text -> Value -> IO Bool
writeNotification state methodName paramsValue =
    writeJsonLine
        state
        DirectionNotification
        ( encode
            ( object
                [ "jsonrpc" .= RPC.rPC_VERSION
                , "method" .= methodName
                , "params" .= paramsValue
                ]
            )
        )

writeJsonLine :: ServerState -> RecordingDirection -> BL8.ByteString -> IO Bool
writeJsonLine state direction bytes = do
    writeResult <-
        tryIOError $ do
            BL8.hPutStr stdout bytes
            BL8.hPutStr stdout "\n"
            hFlush stdout
    case writeResult of
        Left _ -> pure False
        Right () -> do
            recordOutgoingLine state direction bytes
            pure True

recordIncomingRequestLine :: ServerState -> BS.ByteString -> IO ()
recordIncomingRequestLine state line =
    withActiveRecording state $ \recording ->
        appendRecordingEvent
            (activeRecordingHandle recording)
            DirectionRequest
            (TE.decodeUtf8With TEE.lenientDecode line)

recordOutgoingLine :: ServerState -> RecordingDirection -> BL.ByteString -> IO ()
recordOutgoingLine state direction line =
    withActiveRecording state $ \recording ->
        appendRecordingEvent
            (activeRecordingHandle recording)
            direction
            (TE.decodeUtf8With TEE.lenientDecode (BL.toStrict line))

withActiveRecording :: ServerState -> (RecordingSession -> IO ()) -> IO ()
withActiveRecording state action = do
    maybeRecording <- readIORef (stateRecording state)
    case maybeRecording of
        Nothing -> pure ()
        Just recording -> action recording

decodeParamsValue :: (FromJSON a) => Value -> Either RpcFailure a
decodeParamsValue paramsValue =
    case fromJSON paramsValue of
        Error err -> Left (invalidParams err)
        Success value -> Right value

requireNoParamsValue :: Value -> Either RpcFailure ()
requireNoParamsValue paramsValue =
    case paramsValue of
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

rpcFailureToValue :: RpcFailure -> Value
rpcFailureToValue failure =
    object
        [ "code" .= failureCode failure
        , "message" .= failureMessage failure
        , "data" .= failureData failure
        ]

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
    , launchEnv :: Maybe [(String, Maybe String)]
    , launchCwd :: Maybe FilePath
    , launchReadySelector :: Maybe Selector
    , launchReadyTimeoutMs :: Maybe Int
    , launchReadyPollIntervalMs :: Maybe Int
    }

instance FromJSON LaunchParams where
    parseJSON =
        withObject "LaunchParams" $ \o ->
            LaunchParams
                <$> o .: "command"
                <*> o .:? "args" AesonTypes..!= []
                <*> (o .:? "env" >>= traverse parseLaunchEnvObject)
                <*> o .:? "cwd"
                <*> (o .:? "readySelector" >>= traverse parseSelector)
                <*> o .:? "readyTimeoutMs"
                <*> o .:? "readyPollIntervalMs"

parseLaunchEnvObject :: Value -> AesonTypes.Parser [(String, Maybe String)]
parseLaunchEnvObject =
    withObject "launch.env" $ \envObject ->
        mapM parseLaunchEnvPair (KM.toList envObject)

parseLaunchEnvPair :: (K.Key, Value) -> AesonTypes.Parser (String, Maybe String)
parseLaunchEnvPair (key, value) =
    case value of
        Null -> pure (K.toString key, Nothing)
        _ -> do
            textValue <- (AesonTypes.parseJSON value :: AesonTypes.Parser Text)
            pure (K.toString key, Just (T.unpack textValue))

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
    , sendLineExpectAfter :: Maybe Selector
    , sendLineTimeoutMs :: Maybe Int
    , sendLinePollIntervalMs :: Maybe Int
    , sendLineAmbiguityMode :: Maybe Text
    }

instance FromJSON SendLineParams where
    parseJSON =
        withObject "SendLineParams" $ \o ->
            SendLineParams
                <$> o .: "text"
                <*> (o .:? "expectAfter" >>= traverse parseSelector)
                <*> o .:? "timeoutMs"
                <*> o .:? "pollIntervalMs"
                <*> o .:? "ambiguityMode"

data CurrentViewParams = CurrentViewParams
    { currentRowsFilter :: Maybe CurrentViewRowsFilter
    , currentRegionFilter :: Maybe CurrentViewRegionFilter
    , currentEntireRowFilter :: Maybe Int
    , currentEntireColFilter :: Maybe Int
    }

instance FromJSON CurrentViewParams where
    parseJSON value =
        case value of
            Null -> pure defaultCurrentViewParams
            _ ->
                withObject
                    "CurrentViewParams"
                    ( \o ->
                        CurrentViewParams
                            <$> o .:? "rows"
                            <*> o .:? "region"
                            <*> o .:? "entireRow"
                            <*> o .:? "entireCol"
                    )
                    value

defaultCurrentViewParams :: CurrentViewParams
defaultCurrentViewParams =
    CurrentViewParams
        { currentRowsFilter = Nothing
        , currentRegionFilter = Nothing
        , currentEntireRowFilter = Nothing
        , currentEntireColFilter = Nothing
        }

data CurrentViewRowsFilter = CurrentViewRowsFilter
    { currentRowsStart :: Int
    , currentRowsEnd :: Int
    }

instance FromJSON CurrentViewRowsFilter where
    parseJSON =
        withObject "CurrentViewRowsFilter" $ \o ->
            CurrentViewRowsFilter
                <$> o .: "start"
                <*> o .: "end"

data CurrentViewRegionFilter = CurrentViewRegionFilter
    { currentRegionCol :: Int
    , currentRegionRow :: Int
    , currentRegionWidth :: Int
    , currentRegionHeight :: Int
    }

instance FromJSON CurrentViewRegionFilter where
    parseJSON =
        withObject "CurrentViewRegionFilter" $ \o ->
            CurrentViewRegionFilter
                <$> o .: "col"
                <*> o .: "row"
                <*> o .: "width"
                <*> o .: "height"

data DumpFormat
    = DumpAnsi
    | DumpPng
    | DumpBoth
    deriving (Eq, Show)

parseDumpFormat :: Text -> Maybe DumpFormat
parseDumpFormat raw =
    case map toLower (T.unpack (T.strip raw)) of
        "ansi" -> Just DumpAnsi
        "png" -> Just DumpPng
        "both" -> Just DumpBoth
        _ -> Nothing

data DumpViewParams = DumpViewParams
    { dumpName :: Text
    , dumpFormat :: DumpFormat
    , dumpTheme :: Maybe String
    , dumpFontPath :: Maybe FilePath
    , dumpRows :: Maybe Int
    , dumpCols :: Maybe Int
    }

instance FromJSON DumpViewParams where
    parseJSON =
        withObject "DumpViewParams" $ \o -> do
            maybeFormat <- o .:? "format"
            formatValue <-
                case maybeFormat of
                    Nothing -> pure DumpAnsi
                    Just raw ->
                        case parseDumpFormat raw of
                            Just parsed -> pure parsed
                            Nothing -> fail "format must be one of: ansi, png, both"
            DumpViewParams
                <$> o .: "name"
                <*> pure formatValue
                <*> o .:? "theme"
                <*> o .:? "font"
                <*> o .:? "rows"
                <*> o .:? "cols"

data RenderViewParams = RenderViewParams
    { renderViewName :: Text
    , renderViewTheme :: Maybe String
    , renderViewFontPath :: Maybe FilePath
    , renderViewRows :: Maybe Int
    , renderViewCols :: Maybe Int
    }

instance FromJSON RenderViewParams where
    parseJSON =
        withObject "RenderViewParams" $ \o ->
            RenderViewParams
                <$> o .: "name"
                <*> o .:? "theme"
                <*> o .:? "font"
                <*> o .:? "rows"
                <*> o .:? "cols"

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
    , waitAmbiguityMode :: Maybe Text
    }

instance FromJSON WaitForTextParams where
    parseJSON =
        withObject "WaitForTextParams" $ \o ->
            WaitForTextParams
                <$> (o .: "selector" >>= parseSelector)
                <*> o .:? "timeoutMs"
                <*> o .:? "pollIntervalMs"
                <*> o .:? "ambiguityMode"

data WaitUntilParams = WaitUntilParams
    { waitUntilPatternValue :: Text
    , waitUntilTimeoutMs :: Maybe Int
    , waitUntilPollIntervalMs :: Maybe Int
    }

instance FromJSON WaitUntilParams where
    parseJSON =
        withObject "WaitUntilParams" $ \o ->
            WaitUntilParams
                <$> o .: "pattern"
                <*> o .:? "timeoutMs"
                <*> o .:? "pollIntervalMs"

data DiffMode
    = DiffText
    | DiffStyled
    deriving (Eq, Show)

data DiffViewParams = DiffViewParams
    { diffLeftPath :: FilePath
    , diffRightPath :: FilePath
    , diffMode :: DiffMode
    }

instance FromJSON DiffViewParams where
    parseJSON =
        withObject "DiffViewParams" $ \o -> do
            maybeMode <- o .:? "mode"
            modeValue <-
                case maybeMode of
                    Nothing -> pure DiffText
                    Just raw ->
                        case parseDiffMode raw of
                            Just modeValue -> pure modeValue
                            Nothing -> fail "mode must be one of: text, styled"
            DiffViewParams
                <$> o .: "leftPath"
                <*> o .: "rightPath"
                <*> pure modeValue

parseDiffMode :: Text -> Maybe DiffMode
parseDiffMode raw =
    case map toLower (T.unpack (T.strip raw)) of
        "text" -> Just DiffText
        "styled" -> Just DiffStyled
        _ -> Nothing

data ViewSubscribeParams = ViewSubscribeParams
    { subscribeDebounceMs :: Int
    , subscribeIncludeText :: Bool
    }

instance FromJSON ViewSubscribeParams where
    parseJSON value =
        case value of
            Null -> pure defaultViewSubscribeParams
            _ ->
                withObject
                    "ViewSubscribeParams"
                    ( \o ->
                        ViewSubscribeParams
                            <$> o .:? "debounceMs" AesonTypes..!= 100
                            <*> o .:? "includeText" AesonTypes..!= False
                    )
                    value

defaultViewSubscribeParams :: ViewSubscribeParams
defaultViewSubscribeParams =
    ViewSubscribeParams
        { subscribeDebounceMs = 100
        , subscribeIncludeText = False
        }

data BatchParams = BatchParams
    { batchSteps :: [BatchStep]
    }

instance FromJSON BatchParams where
    parseJSON =
        withObject "BatchParams" $ \o ->
            BatchParams
                <$> o .:? "steps" AesonTypes..!= []

data BatchStep = BatchStep
    { batchStepMethod :: Text
    , batchStepParams :: Value
    }

instance FromJSON BatchStep where
    parseJSON =
        withObject "BatchStep" $ \o ->
            BatchStep
                <$> o .: "method"
                <*> o .:? "params" AesonTypes..!= Null

data RecordingStartParams = RecordingStartParams
    { recordingStartPath :: FilePath
    }

instance FromJSON RecordingStartParams where
    parseJSON =
        withObject "RecordingStartParams" $ \o ->
            RecordingStartParams
                <$> o .: "path"

data ReplayParams = ReplayParams
    { replayPath :: FilePath
    , replaySpeed :: ReplaySpeed
    }

instance FromJSON ReplayParams where
    parseJSON =
        withObject "ReplayParams" $ \o -> do
            maybeSpeed <- o .:? "speed"
            speed <-
                case maybeSpeed of
                    Nothing -> pure ReplayAsFastAsPossible
                    Just raw ->
                        case parseReplaySpeed raw of
                            Just speed -> pure speed
                            Nothing -> fail "speed must be one of: as-fast-as-possible, real-time"
            ReplayParams
                <$> o .: "path"
                <*> pure speed

parseReplaySpeed :: Text -> Maybe ReplaySpeed
parseReplaySpeed raw =
    case map toLower (T.unpack (T.strip raw)) of
        "as-fast-as-possible" -> Just ReplayAsFastAsPossible
        "real-time" -> Just ReplayRealTime
        _ -> Nothing

renderReplaySpeed :: ReplaySpeed -> Text
renderReplaySpeed speed =
    case speed of
        ReplayAsFastAsPossible -> "as-fast-as-possible"
        ReplayRealTime -> "real-time"

parseSelector :: Value -> AesonTypes.Parser Selector
parseSelector =
    withObject "Selector" $ \o -> do
        selectorType <- (T.toLower <$> (o .: "type" :: AesonTypes.Parser Text))
        case selectorType of
            "exact" -> Exact <$> o .: "text"
            "regex" -> Regex <$> o .: "pattern"
            "at" -> do
                col <- o .: "col"
                row <- o .: "row"
                if col < 1 || row < 1
                    then fail "selector.at uses 1-based coordinates; col/row must be >= 1"
                    else pure (At (col - 1) (row - 1))
            "within" -> do
                rectValue <- o .: "rect" >>= parseRect
                nested <- o .: "selector" >>= parseSelector
                pure (Within rectValue nested)
            "nth" -> Nth <$> o .: "index" <*> (o .: "selector" >>= parseSelector)
            _ -> fail ("unknown selector type: " <> T.unpack selectorType)

parseRect :: Value -> AesonTypes.Parser Rect
parseRect =
    withObject "Rect" $ \o -> do
        col <- o .: "col"
        row <- o .: "row"
        width <- o .: "width"
        height <- o .: "height"
        if col < 1 || row < 1
            then fail "selector.within.rect uses 1-based col/row coordinates"
            else
                if width < 1 || height < 1
                    then fail "selector.within.rect width/height must be >= 1"
                    else pure (Rect (col - 1) (row - 1) width height)

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
                    Left "ambiguityMode must be one of: fail, first, first-visible, last, last-visible"

parseAmbiguityMode :: Text -> Maybe AmbiguityMode
parseAmbiguityMode raw =
    case map toLower (T.unpack (T.strip raw)) of
        "fail" -> Just FailOnAmbiguous
        "first" -> Just FirstVisibleMatch
        "first-visible" -> Just FirstVisibleMatch
        "last" -> Just LastVisibleMatch
        "last-visible" -> Just LastVisibleMatch
        _ -> Nothing

parseSendKey :: Text -> Either String ([Modifier], Key)
parseSendKey rawKey =
    if trimmed == "+"
        then (,) [] <$> parseBaseKey trimmed
        else case T.breakOn "+" trimmed of
            (_, "") ->
                (,) [] <$> parseBaseKey trimmed
            (modifierText, remainder) ->
                do
                    modifier <- parseModifier (T.strip modifierText)
                    keyValue <- parseModifiedKey (T.strip (T.drop 1 remainder))
                    pure ([modifier], keyValue)
  where
    trimmed = T.strip rawKey

    parseModifier textValue =
        case map toLower (T.unpack textValue) of
            "ctrl" -> Right Control
            "control" -> Right Control
            "alt" -> Right Alt
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

mergeWaitOptions :: WaitOptions -> Maybe Int -> Maybe Int -> WaitOptions
mergeWaitOptions defaults maybeTimeout maybePoll =
    defaults
        { timeoutMs = maybe (timeoutMs defaults) id maybeTimeout
        , pollIntervalMs = maybe (pollIntervalMs defaults) id maybePoll
        }

waitUntilPattern :: Tui -> WaitOptions -> Text -> IO ()
waitUntilPattern tui waitOptions patternText = do
    start <- getCurrentTime
    loop start
  where
    timeoutLimit = fromIntegral (timeoutMs waitOptions) / 1000 :: NominalDiffTime

    loop :: UTCTime -> IO ()
    loop startedAt = do
        viewText <- currentView tui
        if regexLikeMatch patternText viewText
            then pure ()
            else do
                now <- getCurrentTime
                if diffUTCTime now startedAt >= timeoutLimit
                    then throwIO (userError "waitUntil timed out")
                    else do
                        threadDelay (pollIntervalMs waitOptions * 1000)
                        loop startedAt

data CurrentViewFilter
    = FilterFull
    | FilterRows Int Int
    | FilterRegion Int Int Int Int
    | FilterEntireRow (Maybe Int)
    | FilterEntireCol (Maybe Int)

resolveCurrentViewFilter :: CurrentViewParams -> Int -> Int -> Either String CurrentViewFilter
resolveCurrentViewFilter params totalRows totalCols =
    case length activeFilters of
        n | n > 1 -> Left "currentView accepts exactly one of: rows, region, entireRow, entireCol"
        _ ->
            case activeFilters of
                [] -> Right FilterFull
                ["rows"] ->
                    case currentRowsFilter params of
                        Nothing -> Right FilterFull
                        Just rowsFilter -> do
                            start <- normalizeRowIndex "rows.start" totalRows (currentRowsStart rowsFilter)
                            end <- normalizeRowIndex "rows.end" totalRows (currentRowsEnd rowsFilter)
                            if start > end
                                then Left "rows.start must be <= rows.end"
                                else Right (FilterRows start end)
                ["region"] ->
                    case currentRegionFilter params of
                        Nothing -> Right FilterFull
                        Just regionFilter -> resolveRegion regionFilter
                ["entireRow"] ->
                    case currentEntireRowFilter params of
                        Nothing -> Right FilterFull
                        Just rowValue ->
                            if rowValue == 0
                                then Right (FilterEntireRow Nothing)
                                else
                                    if rowValue < 0 || rowValue > totalRows
                                        then Left "entireRow must be 0 or between 1 and total rows"
                                        else Right (FilterEntireRow (Just rowValue))
                ["entireCol"] ->
                    case currentEntireColFilter params of
                        Nothing -> Right FilterFull
                        Just colValue ->
                            if colValue == 0
                                then Right (FilterEntireCol Nothing)
                                else
                                    if colValue < 0 || colValue > totalCols
                                        then Left "entireCol must be 0 or between 1 and total cols"
                                        else Right (FilterEntireCol (Just colValue))
                _ -> Left "invalid currentView filter selection"
  where
    activeFilters :: [Text]
    activeFilters =
        concat
            [ maybe [] (const ["rows"]) (currentRowsFilter params)
            , maybe [] (const ["region"]) (currentRegionFilter params)
            , maybe [] (const ["entireRow"]) (currentEntireRowFilter params)
            , maybe [] (const ["entireCol"]) (currentEntireColFilter params)
            ]

    resolveRegion regionFilter = do
        let rawCol = currentRegionCol regionFilter
        let rawRow = currentRegionRow regionFilter
        let rawWidth = currentRegionWidth regionFilter
        let rawHeight = currentRegionHeight regionFilter

        startCol <-
            if rawCol == 0
                then Right 1
                else normalizeColIndex "region.col" totalCols rawCol

        startRow <-
            if rawRow == 0
                then Right 1
                else normalizeRowIndex "region.row" totalRows rawRow

        endCol <-
            if rawCol == 0
                then Right totalCols
                else
                    if rawWidth <= 0
                        then Left "region.width must be > 0 when region.col is non-zero"
                        else Right (min totalCols (startCol + rawWidth - 1))

        endRow <-
            if rawRow == 0
                then Right totalRows
                else
                    if rawHeight <= 0
                        then Left "region.height must be > 0 when region.row is non-zero"
                        else Right (min totalRows (startRow + rawHeight - 1))

        if startCol > endCol || startRow > endRow
            then Left "resolved region is empty"
            else Right (FilterRegion startCol startRow endCol endRow)

normalizeRowIndex :: String -> Int -> Int -> Either String Int
normalizeRowIndex label totalRows rawValue =
    if rawValue == 0
        then Right 1
        else
            if rawValue < 1 || rawValue > totalRows
                then Left (label <> " must be 0 or between 1 and " <> show totalRows)
                else Right rawValue

normalizeColIndex :: String -> Int -> Int -> Either String Int
normalizeColIndex label totalCols rawValue =
    if rawValue == 0
        then Right 1
        else
            if rawValue < 1 || rawValue > totalCols
                then Left (label <> " must be 0 or between 1 and " <> show totalCols)
                else Right rawValue

applyCurrentViewFilter :: CurrentViewFilter -> Int -> Int -> Text -> (Text, Int, Int)
applyCurrentViewFilter filterValue totalRows totalCols textValue =
    case filterValue of
        FilterFull -> renderGrid grid
        FilterRows start end ->
            let selected = take (end - start + 1) (drop (start - 1) grid)
             in renderGrid selected
        FilterRegion startCol startRow endCol endRow ->
            let selectedRows = take (endRow - startRow + 1) (drop (startRow - 1) grid)
                selectedCols = map (sliceColumns startCol endCol) selectedRows
             in renderGrid selectedCols
        FilterEntireRow maybeRow ->
            case maybeRow of
                Nothing -> renderGrid grid
                Just rowIdx ->
                    case safeIndex (rowIdx - 1) grid of
                        Nothing -> renderGrid []
                        Just rowText -> renderGrid [rowText]
        FilterEntireCol maybeCol ->
            case maybeCol of
                Nothing -> renderGrid grid
                Just colIdx ->
                    let selected = map (sliceColumns colIdx colIdx) grid
                     in renderGrid selected
  where
    grid = normalizeViewportGrid totalRows totalCols textValue

normalizeViewportGrid :: Int -> Int -> Text -> [Text]
normalizeViewportGrid rows cols textValue =
    map normalizeLine [0 .. rows - 1]
  where
    sourceLines = T.lines textValue
    padding = T.replicate cols " "

    normalizeLine idx =
        let sourceLine = fromMaybe "" (safeIndex idx sourceLines)
         in T.take cols (sourceLine <> padding)

renderGrid :: [Text] -> (Text, Int, Int)
renderGrid rowsText =
    let rowCount = length rowsText
        colCount =
            case rowsText of
                [] -> 0
                rowValue : _ -> T.length rowValue
     in (T.intercalate "\n" rowsText, rowCount, colCount)

sliceColumns :: Int -> Int -> Text -> Text
sliceColumns startCol endCol lineText =
    T.take (endCol - startCol + 1) (T.drop (startCol - 1) lineText)

safeIndex :: Int -> [a] -> Maybe a
safeIndex idx values
    | idx < 0 = Nothing
    | otherwise = go idx values
  where
    go _ [] = Nothing
    go 0 (x : _) = Just x
    go n (_ : rest) = go (n - 1) rest

renderSnapshotFromDump :: DumpViewParams -> FilePath -> IO FilePath
renderSnapshotFromDump params ansiPath = do
    let pngPath = defaultPngPath ansiPath
    renderAnsiSnapshotFileWithFont
        (dumpFontPath params)
        (dumpRows params)
        (dumpCols params)
        (dumpTheme params)
        ansiPath
        pngPath
    canonicalizeExistingPath pngPath

renderSnapshotFromRenderView :: RenderViewParams -> FilePath -> IO FilePath
renderSnapshotFromRenderView params ansiPath = do
    let pngPath = defaultPngPath ansiPath
    renderAnsiSnapshotFileWithFont
        (renderViewFontPath params)
        (renderViewRows params)
        (renderViewCols params)
        (renderViewTheme params)
        ansiPath
        pngPath
    canonicalizeExistingPath pngPath

defaultPngPath :: FilePath -> FilePath
defaultPngPath ansiPath =
    if ".ansi.txt" `isSuffixOf` ansiPath
        then take (length ansiPath - length (".ansi.txt" :: String)) ansiPath <> ".png"
        else ansiPath <> ".png"

canonicalizeExistingPath :: FilePath -> IO FilePath
canonicalizeExistingPath path = do
    exists <- doesFileExist path
    if exists
        then canonicalizePath path
        else pure path

snapshotMetaPath :: FilePath -> FilePath
snapshotMetaPath ansiPath =
    if ".ansi.txt" `isSuffixOf` ansiPath
        then take (length ansiPath - length (".ansi.txt" :: String)) ansiPath <> ".meta.json"
        else ansiPath <> ".meta.json"

data SnapshotDiffResult = SnapshotDiffResult
    { diffChanged :: Bool
    , diffChangedLines :: Int
    , diffSummary :: Text
    }

computeSnapshotDiff :: RunOptions -> DiffViewParams -> IO SnapshotDiffResult
computeSnapshotDiff options params = do
    leftPath <- canonicalizeExistingPath (diffLeftPath params)
    rightPath <- canonicalizeExistingPath (diffRightPath params)

    leftExists <- doesFileExist leftPath
    rightExists <- doesFileExist rightPath

    if not leftExists || not rightExists
        then
            throwIO
                ( userError
                    ( "diffView requires both paths to exist (left="
                        <> leftPath
                        <> ", right="
                        <> rightPath
                        <> ")"
                    )
                )
        else do
            leftComparable <- loadComparableSnapshot options (diffMode params) leftPath
            rightComparable <- loadComparableSnapshot options (diffMode params) rightPath
            let changed = leftComparable /= rightComparable
            let changedLineCount = lineDifferenceCount leftComparable rightComparable
            let summary =
                    if changed
                        then
                            "Snapshots differ ("
                                <> T.pack (show changedLineCount)
                                <> " changed lines)"
                        else "Snapshots are identical"
            pure
                SnapshotDiffResult
                    { diffChanged = changed
                    , diffChangedLines = changedLineCount
                    , diffSummary = summary
                    }

loadComparableSnapshot :: RunOptions -> DiffMode -> FilePath -> IO Text
loadComparableSnapshot options mode path = do
    ansiText <- TIO.readFile path
    (rows, cols) <- loadSnapshotDimensions path options
    case mode of
        DiffText -> pure (renderAnsiViewportText rows cols ansiText)
        DiffStyled -> pure (T.pack (serializeAnsiSnapshot rows cols (snapshotTheme options) ansiText))

loadSnapshotDimensions :: FilePath -> RunOptions -> IO (Int, Int)
loadSnapshotDimensions path options = do
    let metaPath = snapshotMetaPath path
    metaExists <- doesFileExist metaPath
    if not metaExists
        then pure (terminalRows options, terminalCols options)
        else do
            metaBytes <- BL.readFile metaPath
            case eitherDecode metaBytes of
                Left _ -> pure (terminalRows options, terminalCols options)
                Right meta -> pure (snapshotMetaRows meta, snapshotMetaCols meta)

data SnapshotMeta = SnapshotMeta
    { snapshotMetaRows :: Int
    , snapshotMetaCols :: Int
    }

instance FromJSON SnapshotMeta where
    parseJSON =
        withObject "SnapshotMeta" $ \o ->
            SnapshotMeta
                <$> o .: "rows"
                <*> o .: "cols"

lineDifferenceCount :: Text -> Text -> Int
lineDifferenceCount left right =
    go 0 leftLines rightLines
  where
    leftLines = T.lines left
    rightLines = T.lines right

    go count [] [] = count
    go count (l : ls) [] = go (if T.null l then count else count + 1) ls []
    go count [] (r : rs) = go (if T.null r then count else count + 1) [] rs
    go count (l : ls) (r : rs) =
        go
            (if l == r then count else count + 1)
            ls
            rs

emitViewChangedNotification :: ServerState -> Tui -> IO ()
emitViewChangedNotification state tui = do
    maybeSubscription <- readIORef (stateViewSubscription state)
    case maybeSubscription of
        Nothing -> pure ()
        Just subscription -> do
            nowMicros <- currentMicros
            textValue <- currentView tui
            let changedSinceLast = maybe True (/= textValue) (subscriptionLastView subscription)
            let debounceMicros = fromIntegral (subscriptionDebounceMs subscription) * 1000
            let pastDebounce =
                    case subscriptionLastSentMicros subscription of
                        Nothing -> True
                        Just lastSent -> nowMicros - lastSent >= debounceMicros
            let updatedSubscription = subscription{subscriptionLastView = Just textValue}
            if changedSinceLast && pastDebounce
                then do
                    let options = tuiOptions tui
                    let payloadBase =
                            [ "rows" .= terminalRows options
                            , "cols" .= terminalCols options
                            ]
                    let payload =
                            if subscriptionIncludeText subscription
                                then object (payloadBase <> ["text" .= textValue])
                                else object payloadBase
                    _ <- writeNotification state "view.changed" payload
                    writeIORef
                        (stateViewSubscription state)
                        (Just updatedSubscription{subscriptionLastSentMicros = Just nowMicros})
                else
                    writeIORef (stateViewSubscription state) (Just updatedSubscription)

currentMicros :: IO Int64
currentMicros = do
    now <- getPOSIXTime
    pure (floor (now * 1000000))

replayRecordedRequestLine :: ServerState -> Text -> IO ()
replayRecordedRequestLine state lineText =
    case eitherDecodeStrict' (TE.encodeUtf8 lineText) :: Either String RPC.JSONRPCRequest of
        Left parseErr ->
            throwIO (userError ("replay failed to parse request line: " <> parseErr))
        Right request ->
            if requestMethod request == "server.shutdown"
                then pure ()
                else case validateRequestVersion request of
                    Left err ->
                        throwIO (userError ("replay request rejected: " <> T.unpack (failureMessage err)))
                    Right () -> do
                        outcome <- dispatchMethod state (requestMethod request) (requestParams request)
                        case outcome of
                            Left failure ->
                                throwIO
                                    ( userError
                                        ( "replay step failed ("
                                            <> T.unpack (requestMethod request)
                                            <> "): "
                                            <> T.unpack (failureMessage failure)
                                        )
                                    )
                            Right _ -> pure ()

regexLikeMatch :: Text -> Text -> Bool
regexLikeMatch patternText haystack =
    any (`wildcardContains` haystack) alternatives
  where
    alternatives =
        filter (not . T.null) $
            map cleanPattern (T.splitOn "|" patternText)

cleanPattern :: Text -> Text
cleanPattern = T.filter (`notElem` ("()" :: String))

wildcardContains :: Text -> Text -> Bool
wildcardContains patternText haystack =
    checkSegments 0 segments
  where
    segments = filter (not . T.null) (T.splitOn ".*" patternText)

    checkSegments :: Int -> [Text] -> Bool
    checkSegments _ [] = True
    checkSegments fromIdx (segment : rest) =
        let remaining = T.drop fromIdx haystack
            (prefix, suffix) = T.breakOn segment remaining
         in if T.null suffix
                then False
                else
                    let nextStart = fromIdx + T.length prefix + T.length segment
                     in checkSegments nextStart rest

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
