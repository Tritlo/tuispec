{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, catch, try)
import Data.Aeson (Value (..), decodeStrict', encode, object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.Environment (getEnvironment, lookupEnv, setEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), Handle, hClose, hFlush, hPutStrLn, hSetBuffering, stderr)
import System.Process (readProcessWithExitCode)
import System.Process qualified as Process
import System.Timeout (timeout)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TuiSpec

main :: IO ()
main =
    defaultMain $
        testGroup
            "tuispec"
            [ tuiTest
                defaultRunOptions
                    { timeoutSeconds = 8
                    , artifactsDir = "artifacts/smoke"
                    , ambiguityMode = FirstVisibleMatch
                    }
                "smoke: text appears in viewport"
                $ \tui -> do
                    launch tui (app "sh" [])
                    sendLine tui "printf 'hello from tuispec\\n'"
                    waitForText tui (Exact "hello from tuispec")
                    expectNotVisible tui (Exact "this text should not exist")
                    expectSnapshot tui "smoke-viewport"
                    sendLine tui "exit"
            , tuiTest
                defaultRunOptions
                    { timeoutSeconds = 8
                    , artifactsDir = "artifacts/artifact-root-smoke"
                    }
                "smoke: artifact root exists before body"
                $ \tui -> do
                    exists <- doesDirectoryExist (artifactRoot tui)
                    assertBool ("artifact root does not exist: " <> artifactRoot tui) exists
            , testCase "smoke: launchAndWait and artifact helpers" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-helpers-smoke"
                        }
                    "helpers-session"
                    ( \tui -> do
                        launchAndWait tui (app "sh" ["-lc", "printf 'READY\\n'; sleep 2"]) (Exact "READY")
                        artifactPath <- writeArtifactFile tui "notes/helper.txt" "hello artifact"
                        artifactPath @?= artifactFile tui "notes/helper.txt"
                        exists <- doesFileExist artifactPath
                        assertBool "writeArtifactFile should create the artifact" exists
                    )
            , testCase "smoke: env file loading expands self references" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-env-file-smoke"
                        }
                    "env-file-session"
                    ( \tui -> do
                        setEnv "TUISPEC_ENV_FILE_VALUE" "base"
                        let envPath = artifactFile tui "test.env"
                        writeFile envPath "export TUISPEC_ENV_FILE_VALUE=\"prefix:$TUISPEC_ENV_FILE_VALUE\"\n"
                        loadEnvFile envPath
                        value <- lookupEnv "TUISPEC_ENV_FILE_VALUE"
                        value @?= Just "prefix:base"
                    )
            , testCase "smoke: trySelectNumberedChoice selects numbered choice" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-menu-smoke"
                        }
                    "menu-session"
                    ( \tui -> do
                        launch
                            tui
                            ( app
                                "sh"
                                [ "-lc"
                                , "stty -icanon min 1 time 0; printf 'SELECT LANGUAGE\\n > 1) Go\\n   2) Python\\n'; key=$(dd bs=1 count=1 2>/dev/null); printf '\\npicked:%s\\n' \"$key\"; sleep 1"
                                ]
                            )
                        selected <- trySelectNumberedChoiceWith tui defaultWaitOptions{timeoutMs = 2000} "SELECT LANGUAGE" "Python"
                        assertBool "trySelectNumberedChoice should find the Python choice" selected
                        waitForText tui (Exact "picked:2")
                    )
            , testCase "smoke: failure bundle and recording helpers" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-diagnostics-smoke"
                        }
                    "diagnostics-session"
                    ( \tui -> do
                        launchAndWait tui (app "sh" ["-lc", "printf 'READY\\n'; sleep 2"]) (Exact "READY")
                        result <- try (withFailureBundle tui "manual-failure" (assertFailure "boom")) :: IO (Either SomeException ())
                        case result of
                            Left _ -> pure ()
                            Right () -> assertFailure "withFailureBundle should rethrow the wrapped assertion"
                        bundleExists <- doesFileExist (artifactFile tui ("failure-bundles" </> "manual-failure.txt"))
                        assertBool "withFailureBundle should write a diagnostic bundle" bundleExists
                        recordingPath <- writeRecording tui "recordings/session.jsonl"
                        recordingExists <- doesFileExist recordingPath
                        assertBool "writeRecording should write JSONL output" recordingExists
                        recordingText <- readFile recordingPath
                        assertBool "recording should contain frame events" ("\"direction\":\"frame\"" `isInfixOf` recordingText)
                    )
            , testCase "smoke: repl session can dump view" $ do
                snapshotPath <-
                    withTuiSession
                        defaultRunOptions
                            { timeoutSeconds = 8
                            , artifactsDir = "artifacts/repl-smoke"
                            }
                        "session"
                        $ \tui -> do
                            launch tui (app "sh" [])
                            sendLine tui "printf 'hello from repl session\\n'"
                            snapshotPath <- dumpView tui "repl-view"
                            sendLine tui "exit"
                            pure snapshotPath
                exists <- doesFileExist snapshotPath
                assertBool "dumpView should write an ansi snapshot file" exists
            , testCase "smoke: launch supports env overrides" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-env-smoke"
                        }
                    "env-session"
                    ( \tui -> do
                        launch
                            tui
                            App
                                { command = "sh"
                                , args = []
                                , env = Just [("TUISPEC_TEST_ENV", Just "hello-env")]
                                , cwd = Nothing
                                }
                        sendLine tui "printf '%s\\n' \"$TUISPEC_TEST_ENV\""
                        waitForText tui (Exact "hello-env")
                        sendLine tui "exit"
                    )
            , testCase "smoke: launches Haskell action under PTY" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-haskell-smoke"
                        }
                    "haskell-session"
                    ( \tui -> do
                        let markerPath = artifactRoot tui </> "haskell-started"
                        launch
                            tui
                            ( haskellApp "haskell-hello" $ do
                                writeFile markerPath "started"
                                hPutStrLn stderr "hello from Haskell app"
                                threadDelay (2 * 1000 * 1000)
                            )
                        markerExists <- waitForFile markerPath
                        assertBool "Haskell action did not write startup marker" markerExists
                        waitForText tui (Exact "hello from Haskell app")
                    )
            , testCase "smoke: Haskell action can run subprocesses" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-haskell-process-smoke"
                        }
                    "haskell-process-session"
                    ( \tui -> do
                        launch
                            tui
                            ( haskellApp "haskell-process" $ do
                                (exitCode, stdoutText, _stderrText) <-
                                    readProcessWithExitCode "sh" ["-c", "printf subprocess-ok"] ""
                                hPutStrLn stderr "before subprocess"
                                hPutStrLn stderr (show exitCode)
                                hPutStrLn stderr stdoutText
                                threadDelay (2 * 1000 * 1000)
                            )
                        waitForText tui (Exact "before subprocess")
                        waitForText tui (Exact "ExitSuccess")
                        waitForText tui (Exact "subprocess-ok")
                    )
            , testCase "smoke: Haskell action honors cwd and env" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-haskell-cwd-env-smoke"
                        }
                    "haskell-cwd-env-session"
                    ( \tui -> do
                        launch
                            tui
                            ( haskellApp "haskell-cwd-env" $ do
                                cwd <- getCurrentDirectory
                                envValue <- lookupEnv "TUISPEC_TEST_ENV"
                                hPutStrLn stderr cwd
                                hPutStrLn stderr (show envValue)
                                threadDelay (2 * 1000 * 1000)
                            )
                                { cwd = Just "/tmp"
                                , env = Just [("TUISPEC_TEST_ENV", Just "hello-env")]
                                }
                        waitForText tui (Exact "/tmp")
                        waitForText tui (Exact "Just \"hello-env\"")
                    )
            , testCase "smoke: waitForText ignores ANSI escape sequences" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-ansi-smoke"
                        , ambiguityMode = FirstVisibleMatch
                        }
                    "ansi-session"
                    ( \tui -> do
                        launch tui (app "sh" [])
                        sendLine tui "printf '\\033[31mstyled text\\033[0m\\n'"
                        waitForText tui (Exact "styled text")
                        expectNotVisible tui (Exact "\ESC[31mstyled text\ESC[0m")
                        sendLine tui "exit"
                    )
            , testCase "smoke: waitForStable detects viewport stability" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/wait-stable-smoke"
                        , ambiguityMode = FirstVisibleMatch
                        }
                    "wait-stable"
                    ( \tui -> do
                        launch tui (app "sh" [])
                        sendLine tui "printf 'stable output\\n'"
                        waitForText tui (Exact "stable output")
                        waitForStable tui defaultWaitOptions{timeoutMs = 5000} 300
                        sendLine tui "exit"
                    )
            , testCase "server: additions end-to-end" testServerAdditions
            , testCase "server: notifications" testServerNotifications
            , testCase "server: recording + replay JSONL + CLI" testServerRecordingReplay
            ]

testServerAdditions :: IO ()
testServerAdditions =
    withRpcServer "additions" [("TUI_UNSET", "from-server")] $ \server -> do
        initResponse <- rpcCall server 1 "initialize" (object ["name" .= ("additions" :: Text), "ambiguityMode" .= ("first" :: Text)])
        _ <- expectResult initResponse

        cwdPath <- canonicalizePath "artifacts/server-tests/additions/cwd"
        createDirectoryIfMissing True cwdPath

        launchResponse <-
            rpcCall
                server
                2
                "launch"
                ( object
                    [ "command" .= ("sh" :: Text)
                    , "args" .= (["-lc", "printf 'READY:%s\\n' \"$PWD\"; exec sh"] :: [String])
                    , "cwd" .= cwdPath
                    , "env"
                        .= object
                            [ "TUI_SET" .= ("value" :: Text)
                            , "TUI_UNSET" .= Null
                            ]
                    , "readySelector" .= exactSelector "READY:"
                    ]
                )
        _ <- expectResult launchResponse

        sendLineResponse <-
            rpcCall
                server
                3
                "sendLine"
                ( object
                    [ "text" .= ("printf '%s|%s\\n' \"$TUI_SET\" \"$TUI_UNSET\"" :: Text)
                    , "expectAfter" .= exactSelector "value|"
                    ]
                )
        _ <- expectResult sendLineResponse

        viewAll <- expectResult =<< rpcCall server 4 "currentView" (object ["entireRow" .= (0 :: Int)])
        rowsAll <- fieldInt "rows" viewAll
        rowsAll @?= 40

        viewRow <- expectResult =<< rpcCall server 5 "currentView" (object ["rows" .= object ["start" .= (1 :: Int), "end" .= (1 :: Int)]])
        rowsSingle <- fieldInt "rows" viewRow
        rowsSingle @?= 1

        viewCol <- expectResult =<< rpcCall server 6 "currentView" (object ["entireCol" .= (1 :: Int)])
        colsSingle <- fieldInt "cols" viewCol
        colsSingle @?= 1

        invalidFilter <- rpcCall server 7 "currentView" (object ["entireRow" .= (1 :: Int), "rows" .= object ["start" .= (1 :: Int), "end" .= (1 :: Int)]])
        _ <- expectError invalidFilter

        waitUntilResp <- rpcCall server 8 "waitUntil" (object ["pattern" .= ("value.*" :: Text)])
        _ <- expectResult waitUntilResp

        dumpResult <-
            expectResult
                =<< rpcCall
                    server
                    9
                    "dumpView"
                    ( object
                        [ "name" .= ("snap-a" :: Text)
                        , "format" .= ("both" :: Text)
                        ]
                    )
        snapA <- fieldText "snapshotPath" dumpResult
        pngA <- fieldText "pngPath" dumpResult
        snapAExists <- doesFileExist (T.unpack snapA)
        pngAExists <- doesFileExist (T.unpack pngA)
        assertBool "dumpView should emit ANSI snapshot path" snapAExists
        assertBool "dumpView format=both should emit PNG" pngAExists

        _ <-
            expectResult
                =<< rpcCall
                    server
                    10
                    "sendLine"
                    ( object
                        [ "text" .= ("echo changed" :: Text)
                        , "expectAfter" .= exactSelector "changed"
                        ]
                    )

        renderResult <- expectResult =<< rpcCall server 11 "renderView" (object ["name" .= ("snap-b" :: Text)])
        snapB <- fieldText "snapshotPath" renderResult
        _ <- fieldText "pngPath" renderResult

        diffChanged <-
            expectResult
                =<< rpcCall
                    server
                    12
                    "diffView"
                    ( object
                        [ "leftPath" .= snapA
                        , "rightPath" .= snapB
                        , "mode" .= ("text" :: Text)
                        ]
                    )
        changedFlag <- fieldBool "changed" diffChanged
        changedFlag @?= True

        diffUnchanged <-
            expectResult
                =<< rpcCall
                    server
                    13
                    "diffView"
                    ( object
                        [ "leftPath" .= snapA
                        , "rightPath" .= snapA
                        ]
                    )
        unchangedFlag <- fieldBool "changed" diffUnchanged
        unchangedFlag @?= False

        _ <- expectResult =<< rpcCall server 14 "sendLine" (object ["text" .= ("echo Specify" :: Text)])
        _ <- expectResult =<< rpcCall server 15 "sendLine" (object ["text" .= ("echo Specify" :: Text)])

        ambiguousFail <-
            rpcCall
                server
                16
                "waitForText"
                ( object
                    [ "selector" .= exactSelector "Specify"
                    , "ambiguityMode" .= ("fail" :: Text)
                    ]
                )
        _ <- expectError ambiguousFail

        ambiguousLast <-
            rpcCall
                server
                17
                "waitForText"
                ( object
                    [ "selector" .= exactSelector "Specify"
                    , "ambiguityMode" .= ("last" :: Text)
                    ]
                )
        _ <- expectResult ambiguousLast

        batchResult <-
            expectResult
                =<< rpcCall
                    server
                    18
                    "batch"
                    ( object
                        [ "steps"
                            .= [ object ["method" .= ("server.ping" :: Text), "params" .= object []]
                               , object ["method" .= ("no.such.method" :: Text), "params" .= object []]
                               ]
                        ]
                    )
        batchOk <- fieldBool "ok" batchResult
        batchOk @?= False
        completedSteps <- fieldInt "completed" batchResult
        completedSteps @?= 1

testServerNotifications :: IO ()
testServerNotifications =
    withRpcServer "notifications" [] $ \server -> do
        _ <- expectResult =<< rpcCall server 1 "initialize" (object ["name" .= ("notify" :: Text)])
        _ <-
            expectResult
                =<< rpcCall
                    server
                    2
                    "launch"
                    ( object
                        [ "command" .= ("sh" :: Text)
                        , "args" .= (["-lc", "cat"] :: [String])
                        ]
                    )

        (_, subscribeNotifications) <-
            rpcCallWithNotifications
                server
                3
                "viewSubscribe"
                ( object
                    [ "debounceMs" .= (0 :: Int)
                    , "includeText" .= True
                    ]
                )
        assertBool "viewSubscribe should emit initial view.changed" (any isViewChangedNotification subscribeNotifications)

        (_, sendNotifications) <- rpcCallWithNotifications server 4 "sendText" (object ["text" .= ("x" :: Text)])
        assertBool "sendText should emit view.changed when subscribed" (any isViewChangedNotification sendNotifications)

        _ <- expectResult =<< rpcCall server 5 "viewUnsubscribe" (object [])
        (_, unsubscribedNotifications) <- rpcCallWithNotifications server 6 "sendText" (object ["text" .= ("y" :: Text)])
        assertBool "sendText should not emit notifications after unsubscribe" (not (any isViewChangedNotification unsubscribedNotifications))

testServerRecordingReplay :: IO ()
testServerRecordingReplay =
    withRpcServer "recording" [] $ \server -> do
        _ <- expectResult =<< rpcCall server 1 "initialize" (object ["name" .= ("recording" :: Text)])
        _ <-
            expectResult
                =<< rpcCall
                    server
                    2
                    "launch"
                    ( object
                        [ "command" .= ("sh" :: Text)
                        , "args" .= (["-lc", "cat"] :: [String])
                        ]
                    )

        let recordingPath = "artifacts/server-tests/recording/session.jsonl"

        _ <- expectResult =<< rpcCall server 3 "recording.start" (object ["path" .= recordingPath])
        _ <- expectResult =<< rpcCall server 4 "sendText" (object ["text" .= ("hello" :: Text)])
        _ <- expectResult =<< rpcCall server 5 "sendKey" (object ["key" .= ("Enter" :: Text)])
        _ <- expectResult =<< rpcCall server 6 "recording.stop" (object [])

        statusResult <- expectResult =<< rpcCall server 7 "recording.status" (object [])
        statusActive <- fieldBool "active" statusResult
        statusActive @?= False

        exists <- doesFileExist recordingPath
        assertBool "recording JSONL should exist" exists

        replayResult <-
            expectResult
                =<< rpcCall
                    server
                    8
                    "replay"
                    ( object
                        [ "path" .= recordingPath
                        , "speed" .= ("as-fast-as-possible" :: Text)
                        ]
                    )
        replayCount <- fieldInt "replayedRequests" replayResult
        assertBool "replay should execute recorded requests" (replayCount >= 2)

        exe <- locateTuiSpecExecutable
        (cliCode, cliOut, cliErr) <-
            Process.readCreateProcessWithExitCode
                (Process.proc exe ["replay", recordingPath, "--speed", "as-fast-as-possible"])
                ""
        case cliCode of
            ExitSuccess ->
                assertBool "CLI replay should report replay progress" ("Replayed" `isInfixOf` cliOut)
            _ ->
                assertFailure ("tuispec replay failed: " <> cliErr)

data RpcServer = RpcServer
    { rpcInput :: Handle
    , rpcOutput :: Handle
    , rpcProcess :: Process.ProcessHandle
    }

withRpcServer :: FilePath -> [(String, String)] -> (RpcServer -> IO a) -> IO a
withRpcServer name extraEnv action = do
    exe <- locateTuiSpecExecutable
    let artifactsRoot = "artifacts/server-tests" </> name
    createDirectoryIfMissing True artifactsRoot
    inheritedEnv <- getEnvironment
    let mergedEnv = applyEnvOverrides inheritedEnv extraEnv
    bracket
        (startServer exe artifactsRoot mergedEnv)
        stopServer
        action

startServer :: FilePath -> FilePath -> [(String, String)] -> IO RpcServer
startServer exe artifactsRoot serverEnv = do
    (Just inputHandle, Just outputHandle, _, processHandle) <-
        Process.createProcess
            (Process.proc exe ["server", "--artifact-dir", artifactsRoot])
                { Process.std_in = Process.CreatePipe
                , Process.std_out = Process.CreatePipe
                , Process.std_err = Process.Inherit
                , Process.env = Just serverEnv
                , Process.create_group = True
                }
    hSetBuffering inputHandle LineBuffering
    hSetBuffering outputHandle LineBuffering
    pure
        RpcServer
            { rpcInput = inputHandle
            , rpcOutput = outputHandle
            , rpcProcess = processHandle
            }

stopServer :: RpcServer -> IO ()
stopServer server = do
    _ <- rpcCall server 9999 "server.shutdown" (object []) `catch` ignoreAny
    hClose (rpcInput server) `catch` ignoreAnyUnit
    Process.terminateProcess (rpcProcess server) `catch` ignoreAnyUnit
    _ <- Process.waitForProcess (rpcProcess server) `catch` ignoreAnyExit
    pure ()

ignoreAny :: SomeException -> IO Value
ignoreAny _ = pure Null

ignoreAnyUnit :: SomeException -> IO ()
ignoreAnyUnit _ = pure ()

ignoreAnyExit :: SomeException -> IO ExitCode
ignoreAnyExit _ = pure ExitSuccess

rpcCall :: RpcServer -> Int -> Text -> Value -> IO Value
rpcCall server reqId methodName paramsValue = do
    (response, _) <- rpcCallWithNotifications server reqId methodName paramsValue
    pure response

rpcCallWithNotifications :: RpcServer -> Int -> Text -> Value -> IO (Value, [Value])
rpcCallWithNotifications server reqId methodName paramsValue = do
    let payload =
            object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "id" .= reqId
                , "method" .= methodName
                , "params" .= paramsValue
                ]
    BS8.hPutStrLn (rpcInput server) (BL.toStrict (encode payload))
    hFlush (rpcInput server)
    readResponses []
  where
    readResponses notifications = do
        maybeLine <- timeout (10 * 1000 * 1000) (BS8.hGetLine (rpcOutput server))
        case maybeLine of
            Nothing ->
                assertFailure ("Timed out waiting for JSON-RPC response for method " <> T.unpack methodName)
            Just line ->
                case decodeStrict' line of
                    Nothing ->
                        assertFailure ("Failed to decode JSON-RPC line: " <> BS8.unpack line)
                    Just value ->
                        if responseHasId reqId value
                            then pure (value, reverse notifications)
                            else
                                if isNotification value
                                    then readResponses (value : notifications)
                                    else readResponses notifications

responseHasId :: Int -> Value -> Bool
responseHasId reqId value =
    case fieldValue "id" value of
        Just (Number n) ->
            case toBoundedInteger n :: Maybe Int of
                Just parsed -> parsed == reqId
                Nothing -> False
        _ -> False

isNotification :: Value -> Bool
isNotification value =
    case value of
        Object _ ->
            case (fieldValue "method" value, fieldValue "id" value) of
                (Just _, Nothing) -> True
                _ -> False
        _ -> False

isViewChangedNotification :: Value -> Bool
isViewChangedNotification value =
    case fieldTextMaybe "method" value of
        Just methodName -> methodName == "view.changed"
        Nothing -> False

expectResult :: Value -> IO Value
expectResult response =
    case fieldValue "result" response of
        Just resultValue -> pure resultValue
        Nothing ->
            case fieldValue "error" response of
                Just errorValue ->
                    assertFailure ("Expected JSON-RPC result but received error: " <> T.unpack (renderValue errorValue))
                Nothing ->
                    assertFailure "Expected JSON-RPC result field"

expectError :: Value -> IO Value
expectError response =
    case fieldValue "error" response of
        Just errorValue -> pure errorValue
        Nothing ->
            assertFailure "Expected JSON-RPC error field"

fieldValue :: Text -> Value -> Maybe Value
fieldValue key value =
    case value of
        Object objectValue -> KM.lookup (K.fromText key) objectValue
        _ -> Nothing

fieldTextMaybe :: Text -> Value -> Maybe Text
fieldTextMaybe key value =
    case fieldValue key value of
        Just (String textValue) -> Just textValue
        _ -> Nothing

fieldText :: Text -> Value -> IO Text
fieldText key value =
    case fieldTextMaybe key value of
        Just textValue -> pure textValue
        Nothing -> assertFailure ("Expected text field: " <> T.unpack key)

fieldInt :: Text -> Value -> IO Int
fieldInt key value =
    case fieldValue key value of
        Just (Number n) ->
            case toBoundedInteger n of
                Just intValue -> pure intValue
                Nothing -> assertFailure ("Expected bounded integer field: " <> T.unpack key)
        _ -> assertFailure ("Expected integer field: " <> T.unpack key)

fieldBool :: Text -> Value -> IO Bool
fieldBool key value =
    case fieldValue key value of
        Just (Bool boolValue) -> pure boolValue
        _ -> assertFailure ("Expected boolean field: " <> T.unpack key)

exactSelector :: Text -> Value
exactSelector textValue =
    object
        [ "type" .= ("exact" :: Text)
        , "text" .= textValue
        ]

renderValue :: Value -> Text
renderValue = TE.decodeUtf8 . BL.toStrict . encode

waitForFile :: FilePath -> IO Bool
waitForFile path = go (20 :: Int)
  where
    go 0 = doesFileExist path
    go remaining = do
        exists <- doesFileExist path
        if exists
            then pure True
            else do
                threadDelay (50 * 1000)
                go (remaining - 1)

locateTuiSpecExecutable :: IO FilePath
locateTuiSpecExecutable = do
    (exitCode, stdoutText, stderrText) <-
        Process.readCreateProcessWithExitCode
            ( Process.proc
                "sh"
                [ "-lc"
                , "find dist-newstyle/build -type f -path '*/x/tuispec/build/tuispec/tuispec' | sort | tail -n 1"
                ]
            )
            ""
    case exitCode of
        ExitSuccess ->
            case lines stdoutText of
                [] -> assertFailure "Could not locate built tuispec executable in dist-newstyle"
                pathValue : _ | null pathValue -> assertFailure "Located empty executable path"
                pathValue : _ -> pure pathValue
        _ -> assertFailure ("Failed to locate tuispec executable: " <> stderrText)

applyEnvOverrides :: [(String, String)] -> [(String, String)] -> [(String, String)]
applyEnvOverrides base overrides =
    foldl
        (\acc (key, value) -> (key, value) : filter ((/= key) . fst) acc)
        base
        overrides
