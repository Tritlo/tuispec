{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TuiSpec.Runner
Description : PTY-backed runner and assertion helpers for TUI tests.

Most users interact with this module through 'tuiTest' and the action/assertion
functions ('launch', 'press', 'waitForText', 'expectSnapshot', ...).
-}
module TuiSpec.Runner (
    closeSession,
    currentView,
    dumpView,
    expectNotVisible,
    expectSnapshot,
    expectVisible,
    launch,
    openSession,
    press,
    pressCombo,
    renderAnsiViewportText,
    sendLine,
    serializeAnsiSnapshot,
    step,
    tuiTest,
    typeText,
    killSessionChildrenNow,
    waitForSelector,
    defaultWaitOptionsFor,
    withTuiSession,
    waitFor,
    waitForText,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException, catch, displayException, finally, throwIO, toException, try)
import Control.Monad (unless, void, when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, isAlphaNum, ord, toLower)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.List (intercalate, isSuffixOf, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removePathForcibly)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath (isRelative, makeRelative, (</>))
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Pty qualified as Pty
import System.Posix.Signals (sigKILL, signalProcess, signalProcessGroup)
import System.Posix.Types (ProcessGroupID)
import System.Process (getPid, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Read (readMaybe)
import TuiSpec.ProjectRoot (resolveProjectRoot)
import TuiSpec.Types

data TestStatus
    = Passed
    | Failed Text
    deriving (Eq, Show)

newtype AssertionError = AssertionError String
    deriving (Show)

instance Exception AssertionError

data StepFailure = StepFailure
    { stepLabel :: String
    , stepAttempts :: Int
    , stepCause :: String
    }
    deriving (Show)

instance Exception StepFailure

settleDelayMicros :: Int
settleDelayMicros = 80 * 1000

{- | Run an ad-hoc PTY session outside @tasty@.

This is useful for REPL-like exploration workflows where you want to:

- launch a TUI once
- drive it with input primitives
- dump intermediate views without snapshot assertions
-}
withTuiSession :: RunOptions -> String -> (Tui -> IO a) -> IO a
withTuiSession options name body = do
    tui <- openSession options name
    body tui `finally` closeSession tui

-- | Open an isolated ad-hoc session TUI handle for interactive orchestration.
openSession :: RunOptions -> String -> IO Tui
openSession options name = do
    envOptions <- applyEnvOverrides options
    projectRoot <- resolveProjectRoot
    artifactBase <- resolveArtifactsBaseDir projectRoot (artifactsDir envOptions)
    let effectiveOptions = envOptions{artifactsDir = artifactBase}
    let sessionRoot = artifactBase </> "sessions" </> slugify name
    resetDirectory sessionRoot
    createDirectoryIfMissing True sessionRoot
    mkTui projectRoot effectiveOptions name sessionRoot sessionRoot 1

-- | Close and teardown a TUI session created with 'openSession'.
closeSession :: Tui -> IO ()
closeSession = teardownTui

-- | Kill the active PTY process group with SIGKILL and return immediately.
killSessionChildrenNow :: Tui -> IO ()
killSessionChildrenNow tui = do
    pty <- readPty tui
    case pty of
        Just ptyHandle -> killPtyProcessGroupNow ptyHandle
        Nothing -> pure ()

{- | Build a @tasty@ 'TestTree' from a TUI spec body.

Each test runs in isolation, with a fresh PTY process and per-test artifacts.
-}
tuiTest :: RunOptions -> String -> (Tui -> IO ()) -> TestTree
tuiTest options name body =
    testCase name $ do
        envOptions <- applyEnvOverrides options
        projectRoot <- resolveProjectRoot
        artifactBase <- resolveArtifactsBaseDir projectRoot (artifactsDir envOptions)
        let effectiveOptions = envOptions{artifactsDir = artifactBase}
        let slug = slugify name
        let testRoot = artifactBase </> "tests" </> slug
        let snapshotRoot = artifactBase </> "snapshots" </> slug
        (status, finalState, attempts) <-
            executeWithRetries projectRoot effectiveOptions (Spec name body) testRoot snapshotRoot
        printTuiTestSummary projectRoot name status attempts testRoot finalState
        case status of
            Passed -> pure ()
            Failed err -> do
                let snapshotEntries = renderSnapshotEntries finalState
                let artifactDetails =
                        if snapshotEntries == "  (none)\n"
                            then ""
                            else
                                "\n\nArtifacts: "
                                    <> makeRelative projectRoot testRoot
                                    <> "\nSnapshots:\n"
                                    <> snapshotEntries
                assertFailure (T.unpack err <> artifactDetails)

{- | Launch an application in a fresh PTY for the current test.

If an app is already running for the test, it is terminated first.
-}
launch :: Tui -> App -> IO ()
launch tui app = do
    appendAction tui ("launch " <> T.pack (command app))
    currentPty <- readPty tui
    case currentPty of
        Just ptyHandle ->
            terminatePtyHandle ptyHandle
        Nothing -> pure ()
    writePty tui Nothing
    maybePty <- initializePty (terminalRows (tuiOptions tui)) (terminalCols (tuiOptions tui)) app
    case maybePty of
        Just ptyHandle -> do
            writePty tui (Just ptyHandle)
            modifyState tui $ \state -> state{launchedApp = Just app}
            threadDelay settleDelayMicros
            _ <- syncVisibleBuffer tui
            pure ()
        Nothing ->
            throwIO (AssertionError "PTY backend unavailable during launch")

-- | Send a single key press to the PTY application.
press :: Tui -> Key -> IO ()
press tui key = do
    appendAction tui ("press " <> renderKey key)
    pty <- readPty tui
    case pty of
        Just ptyHandle -> do
            case keyToPtyText key of
                Just keyBytes -> sendPtyText ptyHandle keyBytes
                Nothing -> pure ()
            threadDelay settleDelayMicros
            _ <- syncVisibleBuffer tui
            pure ()
        Nothing ->
            throwIO (AssertionError "PTY backend unavailable during key press")

{- | Send a modified key press (for example @Ctrl+C@) to the PTY application.

Unsupported combos fall back to a plain 'press'.
-}
pressCombo :: Tui -> [Modifier] -> Key -> IO ()
pressCombo tui modifiers key = do
    appendAction tui $
        "pressCombo "
            <> T.pack (intercalate "+" (map show modifiers))
            <> "+"
            <> renderKey key
    pty <- readPty tui
    case pty of
        Just ptyHandle ->
            case comboToPtyText modifiers key of
                Just token -> do
                    sendPtyText ptyHandle token
                    threadDelay settleDelayMicros
                    _ <- syncVisibleBuffer tui
                    pure ()
                Nothing -> press tui key
        Nothing ->
            throwIO (AssertionError "PTY backend unavailable during combo key press")

-- | Send literal text to the PTY application.
typeText :: Tui -> Text -> IO ()
typeText tui textValue = do
    appendAction tui ("typeText " <> textValue)
    pty <- readPty tui
    case pty of
        Just ptyHandle -> do
            sendPtyText ptyHandle textValue
            threadDelay settleDelayMicros
            _ <- syncVisibleBuffer tui
            pure ()
        Nothing ->
            throwIO (AssertionError "PTY backend unavailable during typeText")

-- | Send a line of text followed by @Enter@.
sendLine :: Tui -> Text -> IO ()
sendLine tui line = do
    typeText tui line
    press tui Enter

-- | Return the current visible viewport text.
currentView :: Tui -> IO Text
currentView = syncVisibleBuffer

-- | Assert that a selector eventually becomes visible.
expectVisible :: Tui -> Selector -> IO ()
expectVisible tui selector = do
    waitFor tui (defaultWaitOptionsFor tui) (selectorMatches selector)
    viewport <- currentViewport tui
    assertNotAmbiguous tui selector viewport

-- | Assert that a selector eventually becomes absent.
expectNotVisible :: Tui -> Selector -> IO ()
expectNotVisible tui selector =
    waitFor tui (defaultWaitOptionsFor tui) (not . selectorMatches selector)

-- | Wait for selector text and apply ambiguity checks.
waitForText :: Tui -> Selector -> IO ()
waitForText tui selector = do
    waitFor tui (defaultWaitOptionsFor tui) (selectorMatches selector)
    viewport <- currentViewport tui
    assertNotAmbiguous tui selector viewport

-- | Wait for a selector with explicit wait options and ambiguity handling.
waitForSelector :: Tui -> WaitOptions -> Selector -> IO ()
waitForSelector tui waitOptions selector = do
    waitFor tui waitOptions (selectorMatches selector)
    viewport <- currentViewport tui
    assertNotAmbiguous tui selector viewport

defaultWaitOptionsFor :: Tui -> WaitOptions
defaultWaitOptionsFor tui =
    defaultWaitOptions
        { timeoutMs = timeoutSeconds (tuiOptions tui) * 1000
        }

-- | Poll until a viewport predicate succeeds or timeout is reached.
waitFor :: Tui -> WaitOptions -> (Viewport -> Bool) -> IO ()
waitFor tui waitOptions predicate = do
    start <- getCurrentTime
    loop start
  where
    timeoutLimit = fromIntegral (timeoutMs waitOptions) / 1000 :: NominalDiffTime

    loop :: UTCTime -> IO ()
    loop startedAt = do
        viewport <- currentViewport tui
        if predicate viewport
            then pure ()
            else do
                now <- getCurrentTime
                if diffUTCTime now startedAt >= timeoutLimit
                    then throwIO (AssertionError "waitFor timed out")
                    else do
                        threadDelay (pollIntervalMs waitOptions * 1000)
                        loop startedAt

{- | Capture and compare the current PTY screen against a named snapshot.

Writes:

- per-run capture: @artifacts/tests/<test>/snapshots/<name>.ansi.txt@
- baseline: @artifacts/snapshots/<test>/<name>.ansi.txt@
- metadata for both paths: @<name>.meta.json@ (rows/cols)
-}
expectSnapshot :: Tui -> SnapshotName -> IO ()
expectSnapshot tui snapshotName = do
    let requestedTheme = snapshotTheme (tuiOptions tui)
    when (not (isKnownSnapshotTheme requestedTheme)) $
        appendWarning
            tui
            ( "Unknown snapshot theme '"
                <> T.pack requestedTheme
                <> "', using "
                <> T.pack (snapshotThemeName defaultSnapshotTheme)
                <> "."
            )
    (snapshotStem, actualAnsi, actualAnsiPath) <- captureSnapshotToDir tui (tuiTestRoot tui </> "snapshots") snapshotName
    let rows = terminalRows (tuiOptions tui)
    let cols = terminalCols (tuiOptions tui)
    let baselineDir = tuiSnapshotRoot tui
    createDirectoryIfMissing True baselineDir

    let baselineAnsiPath = baselineDir </> (snapshotStem <> ".ansi.txt")
    let baselineMetaPath = snapshotMetadataPath baselineAnsiPath

    baselineExists <- doesFileExist baselineAnsiPath
    if updateSnapshots (tuiOptions tui) || not baselineExists
        then do
            TIO.writeFile baselineAnsiPath actualAnsi
            writeSnapshotMetadata baselineMetaPath rows cols
            appendSnapshotArtifact tui baselineAnsiPath
        else do
            baselineAnsi <- TIO.readFile baselineAnsiPath
            let baselineCanonical = serializeAnsiSnapshot rows cols requestedTheme baselineAnsi
            let actualCanonical = serializeAnsiSnapshot rows cols requestedTheme actualAnsi
            if baselineCanonical == actualCanonical
                then do
                    appendSnapshotArtifact tui baselineAnsiPath
                else
                    throwIO $
                        AssertionError
                            ( "Snapshot mismatch for '"
                                <> snapshotStem
                                <> "'. Compare "
                                <> baselineAnsiPath
                                <> " and "
                                <> actualAnsiPath
                                <> ". Render with: tuispec render "
                                <> actualAnsiPath
                            )

{- | Persist the current PTY view as an ANSI snapshot artifact.

This writes only to the active run directory and does not compare against a
baseline, making it suitable for iterative REPL-style exploration.
-}
dumpView :: Tui -> SnapshotName -> IO FilePath
dumpView tui snapshotName = do
    (_stem, _ansi, ansiPath) <- captureSnapshotToDir tui (tuiTestRoot tui </> "snapshots") snapshotName
    pure ansiPath

{- | Canonicalize ANSI text into a theme-aware JSON framebuffer.

This representation is used for deterministic snapshot comparison and PNG
rendering.
-}
serializeAnsiSnapshot :: Int -> Int -> String -> Text -> String
serializeAnsiSnapshot rows cols requestedTheme ansiText =
    let resolvedTheme = resolveSnapshotTheme requestedTheme
        emuState =
            emulateAnsi
                (emptyEmuState rows cols)
                (T.unpack ansiText)
     in serializeSnapshot resolvedTheme emuState

-- | Retry a logical test step according to @StepOptions@.
step :: StepOptions -> String -> IO a -> IO a
step options label action = go 0
  where
    maxAttempts = max 1 (stepMaxRetries options + 1)

    go attempts = do
        result <- try action
        case result of
            Right value -> pure value
            Left (err :: SomeException)
                | attempts + 1 < maxAttempts -> do
                    when (stepRetryDelayMs options > 0) $
                        threadDelay (stepRetryDelayMs options * 1000)
                    go (attempts + 1)
                | otherwise ->
                    throwIO $
                        StepFailure
                            { stepLabel = label
                            , stepAttempts = attempts + 1
                            , stepCause = displayException err
                            }

executeWithRetries ::
    FilePath ->
    RunOptions ->
    Spec ->
    FilePath ->
    FilePath ->
    IO (TestStatus, TuiState, Int)
executeWithRetries projectRoot options specDef testRoot snapshotRoot = go 1
  where
    maxAttempts = max 1 (retries options + 1)
    hardTimeoutMicros = max 1 (timeoutSeconds options) * 1000 * 1000

    go attempt = do
        resetDirectory testRoot
        tui <- mkTui projectRoot options (specName specDef) testRoot snapshotRoot attempt
        runResultAndState <-
            ( do
                maybeRunResult <- timeout hardTimeoutMicros (try (specBody specDef tui))
                let runResult =
                        case maybeRunResult of
                            Nothing ->
                                Left
                                    ( toException
                                        ( AssertionError
                                            ( "test timed out after "
                                                <> show (timeoutSeconds options)
                                                <> "s"
                                            )
                                        )
                                    )
                            Just resultValue -> resultValue
                _ <- timeout (500 * 1000) (syncVisibleBuffer tui)
                state <- readIORef (tuiStateRef tui)
                pure (runResult, state)
            )
                `finally` teardownTui tui
        let (runResult, state) = runResultAndState
        case runResult of
            Right () -> pure (Passed, state, attempt)
            Left (err :: SomeException)
                | attempt < maxAttempts -> go (attempt + 1)
                | otherwise -> pure (Failed (T.pack (displayException err)), state, attempt)

mkTui :: FilePath -> RunOptions -> String -> FilePath -> FilePath -> Int -> IO Tui
mkTui projectRoot options name testRoot snapshotRoot _attempt = do
    ptyRef <- newIORef Nothing
    stateRef <-
        newIORef
            TuiState
                { launchedApp = Nothing
                , visibleBuffer = ""
                , rawBuffer = ""
                , actionLog = []
                , snapshotLog = []
                , runtimeWarnings = []
                , frameLog = []
                }
    tui <-
        pure
            Tui
                { tuiName = name
                , tuiOptions = options
                , tuiRootDir = projectRoot
                , tuiTestRoot = testRoot
                , tuiSnapshotRoot = snapshotRoot
                , tuiPty = ptyRef
                , tuiStateRef = stateRef
                }
    pure tui

initializePty :: Int -> Int -> App -> IO (Maybe PtyHandle)
initializePty rows cols app = do
    result <- try (startPty rows cols app) :: IO (Either SomeException PtyHandle)
    case result of
        Left _ -> pure Nothing
        Right handle -> pure (Just handle)

startPty :: Int -> Int -> App -> IO PtyHandle
startPty rows cols app = do
    processEnv <- withTerminalEnv
    (master, processHandle) <-
        Pty.spawnWithPty
            (Just processEnv)
            True
            (command app)
            (args app)
            (cols, rows)
    pure
        PtyHandle
            { ptyMaster = master
            , ptyProcess = processHandle
            }

withTerminalEnv :: IO [(String, String)]
withTerminalEnv = do
    existing <- getEnvironment
    pure (overrideEnv "TERM" "xterm-256color" existing)
  where
    overrideEnv key value pairs =
        (key, value) : filter ((/= key) . fst) pairs

teardownTui :: Tui -> IO ()
teardownTui tui =
    do
        pty <- readPty tui
        case pty of
            Just ptyHandle ->
                terminatePtyHandle ptyHandle
            _ -> pure ()
        writePty tui Nothing

terminatePtyHandle :: PtyHandle -> IO ()
terminatePtyHandle ptyHandle = do
    _ <- timeout (500 * 1000) (ignoreIOError (terminateProcess (ptyProcess ptyHandle)))
    waitResult <- timeout (500 * 1000) (ignoreIOError (void (waitForProcess (ptyProcess ptyHandle))))
    case waitResult of
        Just () ->
            pure ()
        Nothing -> do
            killPtyProcessGroupNow ptyHandle
            _ <- timeout (500 * 1000) (ignoreIOError (void (waitForProcess (ptyProcess ptyHandle))))
            pure ()
    _ <- timeout (500 * 1000) (ignoreIOError (Pty.closePty (ptyMaster ptyHandle)))
    pure ()

killPtyProcessGroupNow :: PtyHandle -> IO ()
killPtyProcessGroupNow ptyHandle = do
    maybePid <- getPid (ptyProcess ptyHandle)
    case maybePid of
        Nothing -> pure ()
        Just pid -> do
            maybeGroup <- try (getProcessGroupIDOf pid) :: IO (Either SomeException ProcessGroupID)
            case maybeGroup of
                Right groupId ->
                    ignoreIOError (signalProcessGroup sigKILL groupId)
                Left _ ->
                    ignoreIOError (signalProcess sigKILL pid)

readPty :: Tui -> IO (Maybe PtyHandle)
readPty tui = readIORef (tuiPty tui)

writePty :: Tui -> Maybe PtyHandle -> IO ()
writePty tui value = writeIORef (tuiPty tui) value

currentViewport :: Tui -> IO Viewport
currentViewport tui = do
    textValue <- syncVisibleBuffer tui
    pure
        Viewport
            { viewportCols = terminalCols (tuiOptions tui)
            , viewportRows = terminalRows (tuiOptions tui)
            , viewportText = textValue
            }

syncVisibleBuffer :: Tui -> IO Text
syncVisibleBuffer tui = do
    maybePty <- readPty tui
    case maybePty of
        Just ptyHandle -> do
            maybeChunk <- timeout (250 * 1000) (try (drainPtyOutput (ptyMaster ptyHandle)) :: IO (Either SomeException BS.ByteString))
            combinedBytes <-
                case maybeChunk of
                    Nothing -> do
                        appendWarning tui "pty read timed out during viewport sync"
                        pure BS.empty
                    Just (Left outErr) -> do
                        appendWarning tui ("failed to read pty output: " <> T.pack (displayException outErr))
                        pure BS.empty
                    Just (Right outBytes) ->
                        pure outBytes
            if BS.null combinedBytes
                then visibleBuffer <$> readIORef (tuiStateRef tui)
                else do
                    let textChunk = TE.decodeUtf8With TEE.lenientDecode combinedBytes
                    state <- readIORef (tuiStateRef tui)
                    let nextRaw = rawBuffer state <> textChunk
                    let nextVisible =
                            renderAnsiViewportText
                                (terminalRows (tuiOptions tui))
                                (terminalCols (tuiOptions tui))
                                nextRaw
                    modifyState tui $ \st -> st{rawBuffer = nextRaw, visibleBuffer = nextVisible}
                    recordFrame tui nextVisible
                    pure nextVisible
        Nothing ->
            throwIO (AssertionError "PTY backend unavailable during viewport sync")

modifyState :: Tui -> (TuiState -> TuiState) -> IO ()
modifyState tui updateFn = modifyIORef' (tuiStateRef tui) updateFn

appendAction :: Tui -> Text -> IO ()
appendAction tui actionText =
    modifyState tui $ \state ->
        state{actionLog = actionLog state <> [actionText]}

appendSnapshotArtifact :: Tui -> FilePath -> IO ()
appendSnapshotArtifact tui snapshotPath = do
    let relativePath = T.pack (makeRelative (tuiRootDir tui) snapshotPath)
    modifyState tui $ \state ->
        if relativePath `elem` snapshotLog state
            then state
            else state{snapshotLog = snapshotLog state <> [relativePath]}

appendWarning :: Tui -> Text -> IO ()
appendWarning tui warningText =
    modifyState tui $ \state ->
        state{runtimeWarnings = runtimeWarnings state <> [warningText]}

recordFrame :: Tui -> Text -> IO ()
recordFrame tui frameText =
    modifyState tui $ \state ->
        case reverse (frameLog state) of
            latest : _ | latest == frameText -> state
            _ -> state{frameLog = frameLog state <> [frameText]}

renderKey :: Key -> Text
renderKey key =
    case key of
        Enter -> "Enter"
        Esc -> "Esc"
        Tab -> "Tab"
        Backspace -> "Backspace"
        ArrowUp -> "ArrowUp"
        ArrowDown -> "ArrowDown"
        ArrowLeft -> "ArrowLeft"
        ArrowRight -> "ArrowRight"
        Ctrl c -> "Ctrl+" <> T.singleton c
        AltKey c -> "Alt+" <> T.singleton c
        FunctionKey n -> "F" <> T.pack (show n)
        CharKey c -> T.singleton c
        NamedKey name -> name

keyToPtyText :: Key -> Maybe Text
keyToPtyText key =
    case key of
        Enter -> Just "\r"
        Esc -> Just "\ESC"
        Tab -> Just "\t"
        Backspace -> Just "\DEL"
        ArrowUp -> Just "\ESC[A"
        ArrowDown -> Just "\ESC[B"
        ArrowRight -> Just "\ESC[C"
        ArrowLeft -> Just "\ESC[D"
        Ctrl c -> controlChar c
        AltKey c -> Just ("\ESC" <> T.singleton c)
        FunctionKey n -> functionKeySeq n
        CharKey c -> Just (T.singleton c)
        NamedKey value -> Just value
  where
    controlChar c =
        let lowered = toLower c
         in if lowered >= 'a' && lowered <= 'z'
                then Just (T.singleton (chr (ord lowered - ord 'a' + 1)))
                else Nothing

    functionKeySeq n =
        case n of
            1 -> Just "\ESCOP"
            2 -> Just "\ESCOQ"
            3 -> Just "\ESCOR"
            4 -> Just "\ESCOS"
            5 -> Just "\ESC[15~"
            6 -> Just "\ESC[17~"
            7 -> Just "\ESC[18~"
            8 -> Just "\ESC[19~"
            9 -> Just "\ESC[20~"
            10 -> Just "\ESC[21~"
            11 -> Just "\ESC[23~"
            12 -> Just "\ESC[24~"
            _ -> Nothing

comboToPtyText :: [Modifier] -> Key -> Maybe Text
comboToPtyText modifiers key
    | null modifiers = keyToPtyText key
    | otherwise =
        case (modifiers, key) of
            ([Control], CharKey c) ->
                keyToPtyText (Ctrl c)
            ([Alt], CharKey c) ->
                keyToPtyText (AltKey c)
            ([Shift], CharKey c) ->
                keyToPtyText (CharKey (toUpperAscii c))
            _ -> Nothing
  where
    toUpperAscii c
        | c >= 'a' && c <= 'z' = chr (ord c - 32)
        | otherwise = c

sendPtyText :: PtyHandle -> Text -> IO ()
sendPtyText ptyHandle textValue =
    Pty.writePty (ptyMaster ptyHandle) (TE.encodeUtf8 textValue)

drainPtyOutput :: Pty.Pty -> IO BS.ByteString
drainPtyOutput pty = BS.concat . reverse <$> go 0 []
  where
    -- Keep each sync bounded so a constantly-redrawing TUI cannot stall the runner.
    maxDrainChunks :: Int
    maxDrainChunks = 256

    waitChunkMicros :: Int
    waitChunkMicros = 15 * 1000

    go chunkCount acc
        | chunkCount >= maxDrainChunks = pure acc
        | otherwise = do
            ready <- timeout waitChunkMicros (Pty.threadWaitReadPty pty)
            case ready of
                Nothing -> pure acc
                Just () -> do
                    next <- Pty.tryReadPty pty
                    case next of
                        Right chunk
                            | not (BS.null chunk) ->
                                go (chunkCount + 1) (chunk : acc)
                        _ ->
                            pure acc

data EmuColor = EmuColor Int Int Int

data EmuCellStyle = EmuCellStyle
    { cellFg :: Maybe EmuColor
    , cellBg :: Maybe EmuColor
    , cellBold :: Bool
    , cellDim :: Bool
    , cellReverse :: Bool
    }

data EmuCell = EmuCell
    { cellValue :: Char
    , cellStyle :: EmuCellStyle
    }

data EmuState = EmuState
    { emuRows :: Int
    , emuCols :: Int
    , emuCursorRow :: Int
    , emuCursorCol :: Int
    , emuSavedCursor :: Maybe (Int, Int)
    , emuStateStyle :: EmuCellStyle
    , emuCells :: IM.IntMap EmuCell
    }

data SnapshotTheme = PtyDefaultDark | PtyDefaultLight
    deriving (Eq, Show)

data ThemePalette = ThemePalette
    { paletteDefaultBg :: EmuColor
    , paletteDefaultFg :: EmuColor
    , paletteAnsi16 :: [EmuColor]
    }

data SnapshotStyledCell = SnapshotStyledCell
    { styledChar :: Char
    , styledFgColor :: EmuColor
    , styledBgColor :: EmuColor
    }

type SnapshotStyledRow = [SnapshotStyledCell]

defaultCellStyle :: EmuCellStyle
defaultCellStyle =
    EmuCellStyle
        { cellFg = Nothing
        , cellBg = Nothing
        , cellBold = False
        , cellDim = False
        , cellReverse = False
        }

emptyEmuState :: Int -> Int -> EmuState
emptyEmuState rows cols =
    EmuState
        { emuRows = max 1 rows
        , emuCols = max 1 cols
        , emuCursorRow = 0
        , emuCursorCol = 0
        , emuSavedCursor = Nothing
        , emuStateStyle = defaultCellStyle
        , emuCells = IM.empty
        }

defaultSnapshotTheme :: SnapshotTheme
defaultSnapshotTheme = PtyDefaultDark

resolveSnapshotTheme :: String -> SnapshotTheme
resolveSnapshotTheme value =
    if isKnownSnapshotTheme value
        then parseSnapshotTheme value
        else defaultSnapshotTheme

parseSnapshotTheme :: String -> SnapshotTheme
parseSnapshotTheme value =
    case map toLower value of
        "pty-default-dark" -> PtyDefaultDark
        "dark" -> PtyDefaultDark
        "auto" -> defaultSnapshotTheme
        "pty-default-light" -> PtyDefaultLight
        "light" -> PtyDefaultLight
        -- Backward compatibility for previously used labels.
        "github-dark-high-contrast" -> PtyDefaultDark
        "github-light-high-contrast" -> PtyDefaultLight
        _ -> PtyDefaultDark

isKnownSnapshotTheme :: String -> Bool
isKnownSnapshotTheme value =
    case map toLower value of
        "pty-default-dark" -> True
        "dark" -> True
        "auto" -> True
        "pty-default-light" -> True
        "light" -> True
        "github-dark-high-contrast" -> True
        "github-light-high-contrast" -> True
        _ -> False

themePalette :: SnapshotTheme -> ThemePalette
themePalette PtyDefaultDark =
    ThemePalette
        { paletteDefaultBg = EmuColor 0 0 0
        , paletteDefaultFg = EmuColor 229 229 229
        , paletteAnsi16 =
            [ EmuColor 0 0 0
            , EmuColor 205 0 0
            , EmuColor 0 205 0
            , EmuColor 205 205 0
            , EmuColor 0 0 238
            , EmuColor 205 0 205
            , EmuColor 0 205 205
            , EmuColor 229 229 229
            , EmuColor 127 127 127
            , EmuColor 255 0 0
            , EmuColor 0 255 0
            , EmuColor 255 255 0
            , EmuColor 92 92 255
            , EmuColor 255 0 255
            , EmuColor 0 255 255
            , EmuColor 255 255 255
            ]
        }
themePalette PtyDefaultLight =
    ThemePalette
        { paletteDefaultBg = EmuColor 255 255 255
        , paletteDefaultFg = EmuColor 0 0 0
        , paletteAnsi16 =
            [ EmuColor 0 0 0
            , EmuColor 205 0 0
            , EmuColor 0 205 0
            , EmuColor 205 205 0
            , EmuColor 0 0 238
            , EmuColor 205 0 205
            , EmuColor 0 205 205
            , EmuColor 229 229 229
            , EmuColor 127 127 127
            , EmuColor 255 0 0
            , EmuColor 0 255 0
            , EmuColor 255 255 0
            , EmuColor 92 92 255
            , EmuColor 255 0 255
            , EmuColor 0 255 255
            , EmuColor 255 255 255
            ]
        }

{- | Convert ANSI terminal output into the visible viewport text.

This is primarily used by 'tuispec render-text' and snapshot assertions.
-}
renderAnsiViewportText :: Int -> Int -> Text -> Text
renderAnsiViewportText rows cols rawText =
    renderEmuState (emulateAnsi (emptyEmuState rows cols) (T.unpack rawText))

snapshotStyledRows :: SnapshotTheme -> EmuState -> [SnapshotStyledRow]
snapshotStyledRows theme stateValue =
    [ [ snapshotCell row col | col <- [0 .. emuCols stateValue - 1]
      ]
    | row <- [0 .. emuRows stateValue - 1]
    ]
  where
    snapshotCell rowValue colValue =
        maybe
            (defaultStyledCell theme)
            (snapshotCellFromEmuCell theme)
            (IM.lookup (cellIndex stateValue rowValue colValue) (emuCells stateValue))

snapshotCellFromEmuCell :: SnapshotTheme -> EmuCell -> SnapshotStyledCell
snapshotCellFromEmuCell theme emuCell =
    SnapshotStyledCell
        { styledChar = cellValue emuCell
        , styledFgColor = resolveCellFg theme (cellStyle emuCell)
        , styledBgColor = resolveCellBg theme (cellStyle emuCell)
        }

defaultStyledCell :: SnapshotTheme -> SnapshotStyledCell
defaultStyledCell theme =
    let palette = themePalette theme
     in SnapshotStyledCell
            { styledChar = ' '
            , styledFgColor = paletteDefaultFg palette
            , styledBgColor = paletteDefaultBg palette
            }

resolveCellFg :: SnapshotTheme -> EmuCellStyle -> EmuColor
resolveCellFg theme styleValue =
    if cellReverse styleValue
        then fromMaybe (paletteDefaultBg palette) (cellBg styleValue)
        else fromMaybe (paletteDefaultFg palette) (cellFg styleValue)
  where
    palette = themePalette theme

resolveCellBg :: SnapshotTheme -> EmuCellStyle -> EmuColor
resolveCellBg theme styleValue =
    if cellReverse styleValue
        then fromMaybe (paletteDefaultFg palette) (cellFg styleValue)
        else fromMaybe (paletteDefaultBg palette) (cellBg styleValue)
  where
    palette = themePalette theme

emulateAnsi :: EmuState -> String -> EmuState
emulateAnsi stateValue input =
    case input of
        [] -> stateValue
        '\ESC' : '[' : rest ->
            let (payload, remaining) = span (not . isCsiFinal) rest
             in case remaining of
                    [] -> stateValue
                    finalChar : tailChars ->
                        emulateAnsi (applyCsi stateValue payload finalChar) tailChars
        '\ESC' : ']' : rest ->
            emulateAnsi stateValue (dropOsc rest)
        '\ESC' : '7' : rest ->
            emulateAnsi (stateValue{emuSavedCursor = Just (emuCursorRow stateValue, emuCursorCol stateValue)}) rest
        '\ESC' : '8' : rest ->
            emulateAnsi (restoreCursor stateValue) rest
        '\ESC' : 'c' : rest ->
            emulateAnsi (clearScreen (setCursor 0 0 stateValue)) rest
        -- Character set designation escapes (for example ESC ( B) are metadata;
        -- consume them so they do not leak literal bytes into the viewport.
        '\ESC' : '(' : _ : rest ->
            emulateAnsi stateValue rest
        '\ESC' : ')' : _ : rest ->
            emulateAnsi stateValue rest
        '\ESC' : '*' : _ : rest ->
            emulateAnsi stateValue rest
        '\ESC' : '+' : _ : rest ->
            emulateAnsi stateValue rest
        '\ESC' : '-' : _ : rest ->
            emulateAnsi stateValue rest
        '\ESC' : '.' : _ : rest ->
            emulateAnsi stateValue rest
        '\ESC' : _ : rest ->
            emulateAnsi stateValue rest
        '\r' : rest ->
            emulateAnsi (stateValue{emuCursorCol = 0}) rest
        '\n' : rest ->
            emulateAnsi (stateValue{emuCursorRow = clampRow stateValue (emuCursorRow stateValue + 1)}) rest
        '\b' : rest ->
            emulateAnsi (stateValue{emuCursorCol = clampCol stateValue (emuCursorCol stateValue - 1)}) rest
        '\t' : rest ->
            let nextTabStop = ((emuCursorCol stateValue `div` 8) + 1) * 8
             in emulateAnsi (stateValue{emuCursorCol = clampCol stateValue nextTabStop}) rest
        charValue : rest
            | charValue >= ' ' ->
                emulateAnsi (writeCharAtCursor stateValue charValue) rest
            | otherwise ->
                emulateAnsi stateValue rest
  where
    isCsiFinal c = c >= '@' && c <= '~'

dropOsc :: String -> String
dropOsc value =
    case value of
        [] -> []
        '\a' : rest -> rest
        '\ESC' : '\\' : rest -> rest
        _ : rest -> dropOsc rest

applyCsi :: EmuState -> String -> Char -> EmuState
applyCsi stateValue payload finalChar =
    let (isPrivate, paramText) =
            case payload of
                '?' : rest -> (True, rest)
                _ -> (False, payload)
        params = parseCsiParams paramText
        paramAt idx defaultValue = fromMaybe defaultValue (safeIndex idx params)
        modeValue = paramAt 0 0
        amount = max 1 (paramAt 0 1)
     in case finalChar of
            'm' ->
                applySgr stateValue params
            'H' ->
                setCursor (paramAt 0 1 - 1) (paramAt 1 1 - 1) stateValue
            'f' ->
                setCursor (paramAt 0 1 - 1) (paramAt 1 1 - 1) stateValue
            'A' ->
                stateValue{emuCursorRow = clampRow stateValue (emuCursorRow stateValue - amount)}
            'B' ->
                stateValue{emuCursorRow = clampRow stateValue (emuCursorRow stateValue + amount)}
            'C' ->
                stateValue{emuCursorCol = clampCol stateValue (emuCursorCol stateValue + amount)}
            'D' ->
                stateValue{emuCursorCol = clampCol stateValue (emuCursorCol stateValue - amount)}
            'G' ->
                stateValue{emuCursorCol = clampCol stateValue (paramAt 0 1 - 1)}
            'd' ->
                stateValue{emuCursorRow = clampRow stateValue (paramAt 0 1 - 1)}
            'E' ->
                stateValue
                    { emuCursorRow = clampRow stateValue (emuCursorRow stateValue + amount)
                    , emuCursorCol = 0
                    }
            'F' ->
                stateValue
                    { emuCursorRow = clampRow stateValue (emuCursorRow stateValue - amount)
                    , emuCursorCol = 0
                    }
            'J' ->
                clearScreenByMode stateValue modeValue
            'K' ->
                clearLine stateValue modeValue
            's' ->
                stateValue{emuSavedCursor = Just (emuCursorRow stateValue, emuCursorCol stateValue)}
            'u' ->
                restoreCursor stateValue
            'h' ->
                if isPrivate && 1049 `elem` params
                    then clearScreen (setCursor 0 0 stateValue)
                    else stateValue
            'l' ->
                if isPrivate && 1049 `elem` params
                    then clearScreen (setCursor 0 0 stateValue)
                    else stateValue
            _ -> stateValue

applySgr :: EmuState -> [Int] -> EmuState
applySgr stateValue params =
    applySgrParams stateValue (if null params then [0] else params)
  where
    applySgrParams stateAcc [] = stateAcc
    applySgrParams stateAcc (param : rest) =
        case param of
            0 ->
                applySgrParams
                    (stateAcc{emuStateStyle = defaultCellStyle})
                    rest
            1 ->
                applySgrParams
                    (stateAcc{emuStateStyle = (emuStateStyle stateAcc){cellBold = True}})
                    rest
            2 ->
                applySgrParams
                    (stateAcc{emuStateStyle = (emuStateStyle stateAcc){cellDim = True}})
                    rest
            22 ->
                applySgrParams
                    (stateAcc{emuStateStyle = (emuStateStyle stateAcc){cellBold = False, cellDim = False}})
                    rest
            7 ->
                applySgrParams
                    (stateAcc{emuStateStyle = (emuStateStyle stateAcc){cellReverse = True}})
                    rest
            27 ->
                applySgrParams
                    (stateAcc{emuStateStyle = (emuStateStyle stateAcc){cellReverse = False}})
                    rest
            39 -> applySgrParams (clearCellForeground stateAcc) rest
            49 -> applySgrParams (clearCellBackground stateAcc) rest
            x
                | x >= 30 && x <= 37 ->
                    applySgrParams (setCellForeground stateAcc (mapBasicColor (x - 30))) rest
            x
                | x >= 90 && x <= 97 ->
                    applySgrParams (setCellForeground stateAcc (mapBasicColor (x - 90 + 8))) rest
            x
                | x >= 40 && x <= 47 ->
                    applySgrParams (setCellBackground stateAcc (mapBasicColor (x - 40))) rest
            x
                | x >= 100 && x <= 107 ->
                    applySgrParams (setCellBackground stateAcc (mapBasicColor (x - 100 + 8))) rest
            38 ->
                let (nextState, remaining) = parseDynamicColor setCellForeground stateAcc rest
                 in applySgrParams nextState remaining
            48 ->
                let (nextState, remaining) = parseDynamicColor setCellBackground stateAcc rest
                 in applySgrParams nextState remaining
            _ -> applySgrParams stateAcc rest
      where
        clearCellForeground stateNext =
            stateNext{emuStateStyle = (emuStateStyle stateNext){cellFg = Nothing}}
        clearCellBackground stateNext =
            stateNext{emuStateStyle = (emuStateStyle stateNext){cellBg = Nothing}}
        setCellForeground stateNext color =
            stateNext{emuStateStyle = (emuStateStyle stateNext){cellFg = Just color}}
        setCellBackground stateNext color =
            stateNext{emuStateStyle = (emuStateStyle stateNext){cellBg = Just color}}

        parseDynamicColor setFn stateNext values =
            case values of
                [] -> (stateNext, [])
                mode : restValues ->
                    if mode == 2 && length restValues >= 3
                        then
                            let r = colorSafe (restValues !! 0)
                                g = colorSafe (restValues !! 1)
                                b = colorSafe (restValues !! 2)
                             in (setFn stateNext (EmuColor r g b), drop 3 restValues)
                        else case (mode, restValues) of
                            (5, colorValue : tailValues) ->
                                (setFn stateNext (colorFromCode (colorSafe colorValue)), tailValues)
                            _ ->
                                (stateNext, restValues)

        colorSafe value = max 0 (min 255 value)

        mapBasicColor code
            | code >= 0 && code < 16 =
                fromMaybe
                    (EmuColor 0 0 0)
                    (safeIndex code (paletteAnsi16 (themePalette defaultSnapshotTheme)))
            | otherwise = EmuColor 0 0 0

        colorFromCode value =
            EmuColor r g b
          where
            (r, g, b) = ansiColorFromCode value

ansiColorFromCode :: Int -> (Int, Int, Int)
ansiColorFromCode value
    | value >= 0 && value < 16 =
        let EmuColor rValue gValue bValue =
                fromMaybe (EmuColor 0 0 0) (safeIndex value (paletteAnsi16 (themePalette defaultSnapshotTheme)))
         in (rValue, gValue, bValue)
    | value >= 16 && value <= 231 =
        let level v = (v * 51)
            rValue = level ((value - 16) `div` 36)
            gValue = level (((value - 16) `div` 6) `mod` 6)
            bValue = level ((value - 16) `mod` 6)
         in (rValue, gValue, bValue)
    | value >= 232 && value <= 255 =
        let gray = 8 + (value - 232) * 10
         in (gray, gray, gray)
    | otherwise = (0, 0, 0)
parseCsiParams :: String -> [Int]
parseCsiParams value =
    case splitOnSemicolon value of
        [] -> [0]
        parts -> map parsePart parts
  where
    parsePart "" = 0
    parsePart part = fromMaybe 0 (readMaybe part)

splitOnSemicolon :: String -> [String]
splitOnSemicolon value =
    case break (== ';') value of
        (headPart, []) -> [headPart]
        (headPart, _ : rest) -> headPart : splitOnSemicolon rest

setCursor :: Int -> Int -> EmuState -> EmuState
setCursor rowValue colValue stateValue =
    stateValue
        { emuCursorRow = clampRow stateValue rowValue
        , emuCursorCol = clampCol stateValue colValue
        }

restoreCursor :: EmuState -> EmuState
restoreCursor stateValue =
    case emuSavedCursor stateValue of
        Nothing -> stateValue
        Just (rowValue, colValue) ->
            setCursor rowValue colValue stateValue

clearScreen :: EmuState -> EmuState
clearScreen stateValue = stateValue{emuCells = IM.empty}

clearScreenByMode :: EmuState -> Int -> EmuState
clearScreenByMode stateValue modeValue =
    case modeValue of
        1 ->
            deleteCellRange stateValue 0 endIndex
        2 ->
            clearScreen stateValue
        3 ->
            clearScreen stateValue
        _ ->
            deleteCellRange stateValue startIndex lastIndex
  where
    rowValue = emuCursorRow stateValue
    colValue = emuCursorCol stateValue
    cols = emuCols stateValue
    rows = emuRows stateValue
    startIndex = cellIndex stateValue rowValue colValue
    endIndex = cellIndex stateValue rowValue colValue
    lastIndex = rows * cols - 1

deleteCellRange :: EmuState -> Int -> Int -> EmuState
deleteCellRange stateValue startIndex endIndex =
    if endIndex < startIndex
        then stateValue
        else
            stateValue
                { emuCells =
                    foldl'
                        (flip IM.delete)
                        (emuCells stateValue)
                        [startIndex .. endIndex]
                }

clearLine :: EmuState -> Int -> EmuState
clearLine stateValue modeValue =
    stateValue{emuCells = foldl' (flip IM.delete) (emuCells stateValue) lineIndexes}
  where
    rowValue = emuCursorRow stateValue
    colValue = emuCursorCol stateValue
    cols = emuCols stateValue
    indexesFor rangeStart rangeEnd =
        [ cellIndex stateValue rowValue col
        | col <- [rangeStart .. rangeEnd]
        ]
    lineIndexes =
        case modeValue of
            1 -> indexesFor 0 colValue
            2 -> indexesFor 0 (cols - 1)
            _ -> indexesFor colValue (cols - 1)

writeCharAtCursor :: EmuState -> Char -> EmuState
writeCharAtCursor stateValue charValue =
    advanceCursor $
        if not (inBounds stateValue rowValue colValue)
            then stateValue
            else
                let indexValue = cellIndex stateValue rowValue colValue
                    newCell =
                        EmuCell
                            { cellValue = charValue
                            , cellStyle = emuStateStyle stateValue
                            }
                    nextCells =
                        IM.insert indexValue newCell (emuCells stateValue)
                 in stateValue{emuCells = nextCells}
  where
    rowValue = emuCursorRow stateValue
    colValue = emuCursorCol stateValue

    advanceCursor st =
        let nextCol = emuCursorCol st + 1
         in if nextCol >= emuCols st
                then
                    st
                        { emuCursorCol = 0
                        , emuCursorRow = clampRow st (emuCursorRow st + 1)
                        }
                else st{emuCursorCol = nextCol}

inBounds :: EmuState -> Int -> Int -> Bool
inBounds stateValue rowValue colValue =
    rowValue >= 0
        && colValue >= 0
        && rowValue < emuRows stateValue
        && colValue < emuCols stateValue

clampRow :: EmuState -> Int -> Int
clampRow stateValue rowValue =
    max 0 (min (emuRows stateValue - 1) rowValue)

clampCol :: EmuState -> Int -> Int
clampCol stateValue colValue =
    max 0 (min (emuCols stateValue - 1) colValue)

cellIndex :: EmuState -> Int -> Int -> Int
cellIndex stateValue rowValue colValue =
    rowValue * emuCols stateValue + colValue

renderEmuState :: EmuState -> Text
renderEmuState stateValue =
    T.intercalate "\n" (map renderRow [0 .. emuRows stateValue - 1])
  where
    renderRow rowValue =
        T.dropWhileEnd (== ' ') $
            T.pack
                [ maybe ' ' (cellValue) (IM.lookup (cellIndex stateValue rowValue colValue) (emuCells stateValue))
                | colValue <- [0 .. emuCols stateValue - 1]
                ]

serializeSnapshot :: SnapshotTheme -> EmuState -> String
serializeSnapshot theme stateValue =
    "{"
        <> "\"theme\":\""
        <> snapshotThemeName theme
        <> "\","
        <> "\"defaultFg\":"
        <> colorJson (paletteDefaultFg (themePalette theme))
        <> ","
        <> "\"defaultBg\":"
        <> colorJson (paletteDefaultBg (themePalette theme))
        <> ","
        <> "\"rows\":"
        <> show (emuRows stateValue)
        <> ","
        <> "\"cols\":"
        <> show (emuCols stateValue)
        <> ","
        <> "\"cells\":["
        <> intercalate "," (map serializeSnapshotRow (snapshotStyledRows theme stateValue))
        <> "]}"
  where
    serializeSnapshotRow rowValue =
        "["
            <> intercalate "," (map serializeSnapshotCell rowValue)
            <> "]"

    serializeSnapshotCell cellValue =
        "["
            <> intercalate
                ","
                [ show (ord (styledChar cellValue))
                , colorJson (styledFgColor cellValue)
                , colorJson (styledBgColor cellValue)
                ]
            <> "]"

    colorJson colorValue =
        let EmuColor rValue gValue bValue = colorValue
         in "["
                <> intercalate "," (map show [rValue, gValue, bValue])
                <> "]"

snapshotThemeName :: SnapshotTheme -> String
snapshotThemeName PtyDefaultDark = "pty-default-dark"
snapshotThemeName PtyDefaultLight = "pty-default-light"

selectorMatches :: Selector -> Viewport -> Bool
selectorMatches selector viewport =
    case selector of
        Exact textValue -> textValue `T.isInfixOf` viewportText viewport
        Regex patternText -> regexLikeMatch patternText (viewportText viewport)
        At col row ->
            case charAt col row (viewportText viewport) of
                Nothing -> False
                Just c -> c /= ' '
        Within rect nested ->
            selectorMatches nested (viewport{viewportText = cropRect rect (viewportText viewport)})
        Nth idx nested -> matchCount nested viewport > idx

assertNotAmbiguous :: Tui -> Selector -> Viewport -> IO ()
assertNotAmbiguous tui selector viewport =
    when shouldFail $
        throwIO $
            AssertionError
                ( "Ambiguous selector for test '"
                    <> tuiName tui
                    <> "'; matched "
                    <> show totalMatches
                    <> " elements."
                )
  where
    totalMatches = matchCount selector viewport
    mode = ambiguityMode (tuiOptions tui)
    shouldFail =
        mode == FailOnAmbiguous
            && totalMatches > 1
            && not (isExplicit selector)

    isExplicit sel =
        case sel of
            At _ _ -> True
            Nth _ _ -> True
            _ -> False

matchCount :: Selector -> Viewport -> Int
matchCount selector viewport =
    case selector of
        Exact textValue -> occurrenceCount textValue (viewportText viewport)
        Regex patternText ->
            let alternatives = filter (not . T.null) (map cleanPattern (T.splitOn "|" patternText))
             in sum (map (\alt -> regexAlternativeCount alt (viewportText viewport)) alternatives)
        At _ _ ->
            if selectorMatches selector viewport then 1 else 0
        Within _ _ ->
            if selectorMatches selector viewport then 1 else 0
        Nth _ _ ->
            if selectorMatches selector viewport then 1 else 0

regexAlternativeCount :: Text -> Text -> Int
regexAlternativeCount alternative haystack
    | ".*" `T.isInfixOf` alternative =
        if wildcardContains alternative haystack then 1 else 0
    | otherwise = occurrenceCount alternative haystack

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

occurrenceCount :: Text -> Text -> Int
occurrenceCount needle haystack
    | T.null needle = 0
    | otherwise = go 0 haystack
  where
    go count value =
        let (_, after) = T.breakOn needle value
         in if T.null after
                then count
                else go (count + 1) (T.drop (T.length needle) after)

charAt :: Int -> Int -> Text -> Maybe Char
charAt col row textValue
    | col < 0 || row < 0 = Nothing
    | otherwise = do
        line <- safeIndex row (T.lines textValue)
        safeTextIndex col line

cropRect :: Rect -> Text -> Text
cropRect rect textValue =
    T.intercalate "\n" croppedLines
  where
    linesInViewport = T.lines textValue
    y = rectRow rect
    h = rectHeight rect
    x = rectCol rect
    w = rectWidth rect
    selectedRows = take h (drop y linesInViewport)
    croppedLines = map (T.take w . T.drop x) selectedRows

safeIndex :: Int -> [a] -> Maybe a
safeIndex indexValue values
    | indexValue < 0 = Nothing
    | otherwise = go indexValue values
  where
    go _ [] = Nothing
    go 0 (x : _) = Just x
    go n (_ : xs) = go (n - 1) xs

safeTextIndex :: Int -> Text -> Maybe Char
safeTextIndex indexValue textValue
    | indexValue < 0 = Nothing
    | indexValue >= T.length textValue = Nothing
    | otherwise = Just (T.index textValue indexValue)

resolveArtifactsBaseDir :: FilePath -> FilePath -> IO FilePath
resolveArtifactsBaseDir projectRoot value = do
    let basePath =
            if isRelative value
                then projectRoot </> value
                else value
    createDirectoryIfMissing True basePath
    canonicalizePath basePath

printTuiTestSummary ::
    FilePath ->
    String ->
    TestStatus ->
    Int ->
    FilePath ->
    TuiState ->
    IO ()
printTuiTestSummary projectRoot name status attempts testRoot state = do
    let snapshotPaths = sort (nub (map T.unpack (snapshotLog state)))
    let statusLabel =
            case status of
                Passed -> "PASS"
                Failed _ -> "FAIL"
    putStrLn
        ( "[tuispec] "
            <> statusLabel
            <> " "
            <> name
            <> " (attempts="
            <> show attempts
            <> ")"
        )
    unless (null snapshotPaths) $
        do
            putStrLn ("[tuispec] artifacts: " <> makeRelative projectRoot testRoot)
            putStrLn ("[tuispec] snapshots:\n" <> renderSnapshotEntries state)

renderSnapshotEntries :: TuiState -> String
renderSnapshotEntries state =
    case nub (map T.unpack (snapshotLog state)) of
        [] -> "  (none)\n"
        paths ->
            concatMap (\path -> "  - " <> path <> "\n") (sort paths)

resetDirectory :: FilePath -> IO ()
resetDirectory dir = do
    exists <- doesDirectoryExist dir
    when exists (removePathForcibly dir)

safeFileStem :: String -> String
safeFileStem input =
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

slugify :: String -> String
slugify = safeFileStem

snapshotMetadataPath :: FilePath -> FilePath
snapshotMetadataPath ansiPath =
    if ".ansi.txt" `isSuffixOf` ansiPath
        then take (length ansiPath - length (".ansi.txt" :: String)) ansiPath <> ".meta.json"
        else ansiPath <> ".meta.json"

writeSnapshotMetadata :: FilePath -> Int -> Int -> IO ()
writeSnapshotMetadata metaPath rows cols =
    BL.writeFile
        metaPath
        ( encode
            ( object
                [ "rows" .= rows
                , "cols" .= cols
                ]
            )
        )

captureSnapshotToDir :: Tui -> FilePath -> SnapshotName -> IO (String, Text, FilePath)
captureSnapshotToDir tui targetDir snapshotName = do
    when (T.null (unSnapshotName snapshotName)) $
        throwIO (AssertionError "snapshot name cannot be empty")
    _ <- syncVisibleBuffer tui
    state <- readIORef (tuiStateRef tui)
    let actualAnsi = rawBuffer state
    let rows = terminalRows (tuiOptions tui)
    let cols = terminalCols (tuiOptions tui)
    let snapshotStem = safeFileStem (T.unpack (unSnapshotName snapshotName))
    createDirectoryIfMissing True targetDir
    let ansiPath = targetDir </> (snapshotStem <> ".ansi.txt")
    let metaPath = snapshotMetadataPath ansiPath
    TIO.writeFile ansiPath actualAnsi
    writeSnapshotMetadata metaPath rows cols
    appendSnapshotArtifact tui ansiPath
    pure (snapshotStem, actualAnsi, ansiPath)

applyEnvOverrides :: RunOptions -> IO RunOptions
applyEnvOverrides options = do
    timeoutValue <- envInt "TUISPEC_TIMEOUT_SECONDS"
    retriesValue <- envInt "TUISPEC_RETRIES"
    stepRetriesValue <- envInt "TUISPEC_STEP_RETRIES"
    colsValue <- envInt "TUISPEC_TERMINAL_COLS"
    rowsValue <- envInt "TUISPEC_TERMINAL_ROWS"
    artifactsValue <- lookupEnv "TUISPEC_ARTIFACTS_DIR"
    updateValue <- envBool "TUISPEC_UPDATE_SNAPSHOTS"
    ambiguityValue <- envAmbiguity "TUISPEC_AMBIGUITY_MODE"
    snapshotThemeValue <- lookupEnv "TUISPEC_SNAPSHOT_THEME"
    colorFgBgValue <- lookupEnv "COLORFGBG"
    let requestedTheme = fromMaybeString (snapshotTheme options) snapshotThemeValue
        effectiveTheme = resolveAutoSnapshotTheme requestedTheme colorFgBgValue
    pure
        options
            { timeoutSeconds = fromMaybeInt (timeoutSeconds options) timeoutValue
            , retries = fromMaybeInt (retries options) retriesValue
            , stepRetries = fromMaybeInt (stepRetries options) stepRetriesValue
            , terminalCols = fromMaybeInt (terminalCols options) colsValue
            , terminalRows = fromMaybeInt (terminalRows options) rowsValue
            , artifactsDir = fromMaybeString (artifactsDir options) artifactsValue
            , updateSnapshots = fromMaybeBool (updateSnapshots options) updateValue
            , ambiguityMode = fromMaybeAmbiguity (ambiguityMode options) ambiguityValue
            , snapshotTheme = effectiveTheme
            }
  where
    fromMaybeInt fallback maybeValue = maybe fallback id maybeValue
    fromMaybeBool fallback maybeValue = maybe fallback id maybeValue
    fromMaybeString fallback maybeValue = maybe fallback id maybeValue
    fromMaybeAmbiguity fallback maybeValue = maybe fallback id maybeValue

envInt :: String -> IO (Maybe Int)
envInt key = do
    value <- lookupEnv key
    pure (value >>= readMaybe)

envBool :: String -> IO (Maybe Bool)
envBool key = do
    value <- lookupEnv key
    pure (value >>= parseBool)
  where
    parseBool raw =
        case map toLower raw of
            "1" -> Just True
            "true" -> Just True
            "yes" -> Just True
            "on" -> Just True
            "0" -> Just False
            "false" -> Just False
            "no" -> Just False
            "off" -> Just False
            _ -> Nothing

envAmbiguity :: String -> IO (Maybe AmbiguityMode)
envAmbiguity key = do
    value <- lookupEnv key
    pure (value >>= parseAmbiguity)
  where
    parseAmbiguity raw =
        case map toLower raw of
            "fail" -> Just FailOnAmbiguous
            "first" -> Just FirstVisibleMatch
            "first-visible" -> Just FirstVisibleMatch
            _ -> Nothing

resolveAutoSnapshotTheme :: String -> Maybe String -> String
resolveAutoSnapshotTheme requestedTheme colorFgBgValue =
    case map toLower requestedTheme of
        "auto" ->
            case detectTerminalBackground colorFgBgValue of
                Just "light" -> "pty-default-light"
                _ -> "pty-default-dark"
        _ -> requestedTheme

detectTerminalBackground :: Maybe String -> Maybe String
detectTerminalBackground colorFgBgValue =
    case colorFgBgValue >>= parseBgIndex of
        Just bgIndex | bgIndex >= 7 -> Just "light"
        Just _ -> Just "dark"
        Nothing -> Nothing
  where
    parseBgIndex raw =
        case reverse (splitOn ';' raw) of
            [] -> Nothing
            lastPart : _ -> (readMaybe lastPart :: Maybe Int)

    splitOn _ [] = [""]
    splitOn delimiter input =
        case break (== delimiter) input of
            (headPart, []) -> [headPart]
            (headPart, _ : rest) -> headPart : splitOn delimiter rest

ignoreIOError :: IO () -> IO ()
ignoreIOError action =
    action `catch` \(_ :: SomeException) -> pure ()
