{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TuiSpec.Runner
Description : PTY-backed runner and assertion helpers for TUI tests.

Most users interact with this module through 'tuiTest' and the action/assertion
functions ('launch', 'press', 'waitForText', 'expectSnapshot', ...).
-}
module TuiSpec.Runner (
    artifactFile,
    artifactRoot,
    click,
    clickWith,
    clickSelector,
    clickSelectorWith,
    closeSession,
    currentView,
    currentViewRect,
    dumpFailureBundle,
    dumpView,
    expectNotVisible,
    expectSnapshot,
    expectVisible,
    launch,
    launchAndWait,
    launchAndWaitWith,
    loadEnvFile,
    openSession,
    press,
    pressCombo,
    prependPathEntry,
    renderAnsiViewportText,
    selectorOrigin,
    sendLine,
    serializeAnsiSnapshot,
    step,
    tuiTest,
    typeText,
    withFailureBundle,
    writeArtifactFile,
    writeRecording,
    killSessionChildrenNow,
    waitForSelector,
    waitForSelectorWithAmbiguity,
    defaultWaitOptionsFor,
    withTuiSession,
    waitFor,
    waitForStable,
    waitForText,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException, displayException, finally, fromException, throwIO, toException, try)
import Control.Monad (forM_, unless, when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, ord, toLower)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IM
import Data.List (intercalate, nub, sort)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removePathForcibly)
import System.Environment (getEnvironment, lookupEnv, setEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (isRelative, makeRelative, searchPathSeparator, splitSearchPath, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Env qualified as PosixEnv
import System.Posix.IO (OpenMode (ReadWrite), closeFd, defaultFileFlags, dupTo, openFd, stdError, stdInput, stdOutput)
import System.Posix.Process (createSession, exitImmediately, forkProcess, getProcessGroupIDOf)
import System.Posix.Pty qualified as Pty
import System.Posix.Signals (sigKILL, signalProcess, signalProcessGroup)
import System.Posix.Terminal (getSlaveTerminalName, openPseudoTerminal)
import System.Posix.Types (Fd (..), ProcessGroupID)
import System.Process (ProcessHandle, getPid, getProcessExitCode, terminateProcess)
import System.Process.Internals (mkProcessHandle)
import System.Timeout (timeout)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Read (readMaybe)
import TuiSpec.Internal (cleanPattern, ignoreIOError, regexLikeMatch, regexLikeMatchOrigin, resolveAutoSnapshotTheme, safeFileStem, safeIndex, snapshotMetadataPath, wildcardContains)
import TuiSpec.ProjectRoot (resolveProjectRoot)
import TuiSpec.Replay (RecordingDirection (DirectionFrame), RecordingEvent (..))
import TuiSpec.Types

foreign import ccall unsafe "tuispec_enable_packet_mode"
    c_tuispec_enable_packet_mode :: Fd -> IO CInt

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
    mkTui projectRoot effectiveOptions name sessionRoot sessionRoot

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
launch tui appSpec = do
    appendAction tui ("launch " <> appLabel appSpec)
    currentPty <- readPty tui
    case currentPty of
        Just ptyHandle ->
            terminatePtyHandle ptyHandle
        Nothing -> pure ()
    writePty tui Nothing
    ptyResult <- initializePty (terminalRows (tuiOptions tui)) (terminalCols (tuiOptions tui)) appSpec
    case ptyResult of
        Right ptyHandle -> do
            writePty tui (Just ptyHandle)
            modifyState tui $ \state -> state{launchedApp = Just appSpec}
            threadDelay settleDelayMicros
            _ <- syncVisibleBuffer tui
            pure ()
        Left err ->
            throwIO
                ( AssertionError
                    ( "PTY backend unavailable during launch: "
                        <> displayException err
                    )
                )

-- | Launch an application and wait for a ready selector using test defaults.
launchAndWait :: Tui -> App -> Selector -> IO ()
launchAndWait tui appSpec readySelector =
    launchAndWaitWith tui (defaultWaitOptionsFor tui) appSpec readySelector

-- | Launch an application and wait for a ready selector with explicit wait options.
launchAndWaitWith :: Tui -> WaitOptions -> App -> Selector -> IO ()
launchAndWaitWith tui waitOptions appSpec readySelector = do
    launch tui appSpec
    waitForSelector tui waitOptions readySelector

-- | Send a single key press to the PTY application.
press :: Tui -> Key -> IO ()
press tui key = do
    appendAction tui ("press " <> renderKey key)
    failIfLaunchedProcessExited tui "press"
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
    failIfLaunchedProcessExited tui "pressCombo"
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

{- | Left-click at a viewport coordinate.

Coordinates are 0-based, matching the 'At' selector (the top-left cell is
@0 0@). Uses SGR mouse encoding with the left button; see 'clickWith' for
explicit button and encoding control.

The target application only reacts to clicks if it has enabled mouse tracking
(anything built on @vty@\/Brick does once mouse mode is turned on). A click on
an application without mouse tracking is silently ignored by that application.
-}
click :: Tui -> Int -> Int -> IO ()
click tui = clickWith tui defaultClickOptions

{- | Click at a viewport coordinate with explicit button and encoding.

Coordinates are 0-based. A click emits both a button press and a release so
that applications reacting to either edge observe the event.
-}
clickWith :: Tui -> ClickOptions -> Int -> Int -> IO ()
clickWith tui options col row = do
    appendAction tui $
        "click "
            <> T.pack (show (clickButton options))
            <> " "
            <> T.pack (show col)
            <> ","
            <> T.pack (show row)
    failIfLaunchedProcessExited tui "click"
    pty <- readPty tui
    case pty of
        Just ptyHandle -> do
            sendPtyBytes ptyHandle (encodeMouseClick options (col + 1) (row + 1))
            threadDelay settleDelayMicros
            _ <- syncVisibleBuffer tui
            pure ()
        Nothing ->
            throwIO (AssertionError "PTY backend unavailable during click")

{- | Click on the element matched by a selector (Playwright-style).

The selector is resolved against the current viewport to its match origin and
that coordinate is clicked. Honors the run's 'ambiguityMode': under
'FailOnAmbiguous' a non-explicit selector matching more than one element
fails. Resolves to the first match's origin (matching is line-oriented, so a
match spanning a line boundary is reported as no match). Uses SGR mouse
encoding; see 'clickSelectorWith' for button and encoding control.
-}
clickSelector :: Tui -> Selector -> IO ()
clickSelector tui = clickSelectorWith tui defaultClickOptions

-- | Click on the element matched by a selector with explicit button and encoding.
clickSelectorWith :: Tui -> ClickOptions -> Selector -> IO ()
clickSelectorWith tui options selector = do
    appendAction tui ("clickSelector " <> T.pack (show selector))
    failIfLaunchedProcessExited tui "clickSelector"
    viewportTextValue <- syncVisibleBuffer tui
    let runOptions = tuiOptions tui
        viewport =
            Viewport
                { viewportCols = terminalCols runOptions
                , viewportRows = terminalRows runOptions
                , viewportText = viewportTextValue
                }
    assertNotAmbiguousWithMode (tuiName tui) (ambiguityMode runOptions) selector viewport
    case selectorOrigin selector viewport of
        Just (col, row) -> clickWith tui options col row
        Nothing ->
            throwIO
                ( AssertionError
                    ( "clickSelector found no match for selector in test '"
                        <> tuiName tui
                        <> "'."
                    )
                )

-- | Send literal text to the PTY application.
typeText :: Tui -> Text -> IO ()
typeText tui textValue = do
    appendAction tui ("typeText " <> textValue)
    failIfLaunchedProcessExited tui "typeText"
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

-- | Load simple @KEY=value@ entries from an env file into the current process.
loadEnvFile :: FilePath -> IO ()
loadEnvFile path = do
    exists <- doesFileExist path
    when exists $ do
        contents <- TIO.readFile path
        forM_ (T.lines contents) loadLine
  where
    loadLine rawLine = do
        let trimmed = T.strip rawLine
        when (not (T.null trimmed) && not ("#" `T.isPrefixOf` trimmed)) $ do
            let assignment = T.strip $ fromMaybe trimmed (T.stripPrefix "export " trimmed)
            case T.breakOn "=" assignment of
                (key, valueWithEquals)
                    | not (T.null key) && "=" `T.isPrefixOf` valueWithEquals -> do
                        let keyText = T.strip key
                        let keyString = T.unpack keyText
                        oldValue <- fromMaybe "" <$> lookupEnv keyString
                        let rawValue = stripEnvQuotes (T.drop 1 valueWithEquals)
                        setEnv keyString (T.unpack (expandEnvSelfReference keyText (T.pack oldValue) rawValue))
                _ -> pure ()

stripEnvQuotes :: Text -> Text
stripEnvQuotes value =
    if T.length stripped >= 2
        then case (T.head stripped, T.last stripped) of
            ('"', '"') -> T.init (T.tail stripped)
            ('\'', '\'') -> T.init (T.tail stripped)
            _ -> stripped
        else stripped
  where
    stripped = T.strip value

expandEnvSelfReference :: Text -> Text -> Text -> Text
expandEnvSelfReference key oldValue =
    T.replace ("$" <> key) oldValue
        . T.replace ("${" <> key <> "}") oldValue

-- | Prepend a directory to @PATH@ unless it is already present.
prependPathEntry :: FilePath -> IO ()
prependPathEntry dir = do
    oldPath <- fromMaybe "" <$> lookupEnv "PATH"
    let entries = splitSearchPath oldPath
    unless (dir `elem` entries) $
        setEnv "PATH" (intercalate [searchPathSeparator] (dir : filter (not . null) entries))

-- | Return the current visible viewport text.
currentView :: Tui -> IO Text
currentView = syncVisibleBuffer

-- | Return the current viewport text cropped to the given rectangle.
currentViewRect :: Tui -> Rect -> IO Text
currentViewRect tui rect = do
    textValue <- syncVisibleBuffer tui
    pure (cropRect rect textValue)

-- | Assert that a selector eventually becomes visible.
expectVisible :: Tui -> Selector -> IO ()
expectVisible = waitForText

-- | Assert that a selector eventually becomes absent.
expectNotVisible :: Tui -> Selector -> IO ()
expectNotVisible tui selector =
    waitFor tui (defaultWaitOptionsFor tui) (not . selectorMatches selector)

-- | Wait for selector text and apply ambiguity checks.
waitForText :: Tui -> Selector -> IO ()
waitForText tui = waitForSelector tui (defaultWaitOptionsFor tui)

-- | Wait for a selector with explicit wait options and ambiguity handling.
waitForSelector :: Tui -> WaitOptions -> Selector -> IO ()
waitForSelector tui waitOptions selector = do
    waitForSelectorWithAmbiguity tui waitOptions Nothing selector

-- | Wait for a selector with explicit wait and optional ambiguity mode override.
waitForSelectorWithAmbiguity :: Tui -> WaitOptions -> Maybe AmbiguityMode -> Selector -> IO ()
waitForSelectorWithAmbiguity tui waitOptions ambiguityOverride selector = do
    waitFor tui waitOptions (selectorMatches selector)
    viewport <- currentViewport tui
    assertNotAmbiguousWithMode (tuiName tui) effectiveMode selector viewport
  where
    effectiveMode =
        case ambiguityOverride of
            Just overrideMode -> overrideMode
            Nothing -> ambiguityMode (tuiOptions tui)

-- | Derive default wait settings for a TUI from its configured run options.
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
                failIfLaunchedProcessExited tui "waitFor"
                now <- getCurrentTime
                if diffUTCTime now startedAt >= timeoutLimit
                    then throwIO (AssertionError "waitFor timed out")
                    else do
                        threadDelay (pollIntervalMs waitOptions * 1000)
                        loop startedAt

{- | Wait until the viewport text has not changed for @debounceMs@ milliseconds.

This replaces brittle fixed @threadDelay@ calls with a semantic stability
check: the viewport is polled at @pollIntervalMs@ intervals, and the call
returns once the visible text has remained identical for at least @debounceMs@
consecutive milliseconds. Throws on overall timeout.
-}
waitForStable :: Tui -> WaitOptions -> Int -> IO ()
waitForStable tui waitOptions debounceMs = do
    start <- getCurrentTime
    initialView <- syncVisibleBuffer tui
    loop start initialView start
  where
    timeoutLimit = fromIntegral (timeoutMs waitOptions) / 1000 :: NominalDiffTime
    debounceLimit = fromIntegral debounceMs / 1000 :: NominalDiffTime

    loop :: UTCTime -> Text -> UTCTime -> IO ()
    loop startedAt lastView lastChangedAt = do
        threadDelay (pollIntervalMs waitOptions * 1000)
        now <- getCurrentTime
        if diffUTCTime now startedAt >= timeoutLimit
            then throwIO (AssertionError "waitForStable timed out")
            else do
                currentText <- syncVisibleBuffer tui
                let changedAt = if currentText /= lastView then now else lastChangedAt
                if diffUTCTime now changedAt >= debounceLimit
                    then pure ()
                    else do
                        failIfLaunchedProcessExited tui "waitForStable"
                        loop startedAt currentText changedAt

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

-- | Per-test artifact directory for logs and other auxiliary files.
artifactRoot :: Tui -> FilePath
artifactRoot = tuiTestRoot

-- | Resolve a relative path underneath the per-test artifact directory.
artifactFile :: Tui -> FilePath -> FilePath
artifactFile tui path =
    if isRelative path
        then artifactRoot tui </> path
        else path

-- | Write a UTF-8 text artifact and return its absolute path.
writeArtifactFile :: Tui -> FilePath -> Text -> IO FilePath
writeArtifactFile tui path textValue = do
    let fullPath = artifactFile tui path
    createDirectoryIfMissing True (takeDirectory fullPath)
    TIO.writeFile fullPath textValue
    pure fullPath

{- | Persist the captured frame log as a JSONL recording compatible with replay.

Frames are recorded each time the visible buffer changes during the test, with
wall-clock microseconds since the session began. Replay with
@tuispec replay PATH@ to render the trace on a terminal.
-}
writeRecording :: Tui -> FilePath -> IO FilePath
writeRecording tui path = do
    state <- readIORef (tuiStateRef tui)
    let fullPath = artifactFile tui path
    createDirectoryIfMissing True (takeDirectory fullPath)
    BL.writeFile fullPath (BL.concat (map ((<> "\n") . encodeFrame) (reverse (frameLog state))))
    pure fullPath
  where
    encodeFrame :: (Int64, Text) -> BL.ByteString
    encodeFrame (timestampMicros, frameText) =
        encode
            RecordingEvent
                { recordingTimestampMicros = timestampMicros
                , recordingDirection = DirectionFrame
                , recordingLine = frameText
                }

-- | Dump a diagnostic bundle for the current TUI state and return its path.
dumpFailureBundle :: Tui -> SnapshotName -> IO FilePath
dumpFailureBundle tui bundleName = do
    let bundleStem = safeFileStem (T.unpack (unSnapshotName bundleName))
    snapshotResult <- try (dumpView tui bundleName) :: IO (Either SomeException FilePath)
    viewResult <- try (currentView tui) :: IO (Either SomeException Text)
    exitResult <- launchedExitStatus tui
    state <- readIORef (tuiStateRef tui)
    writeArtifactFile
        tui
        ("failure-bundles" </> bundleStem <> ".txt")
        (renderFailureBundle state snapshotResult viewResult exitResult)

-- | Add a failure bundle when an action throws, then rethrow the original error.
withFailureBundle :: Tui -> SnapshotName -> IO a -> IO a
withFailureBundle tui bundleName action = do
    result <- try action
    case result of
        Right value -> pure value
        Left (err :: SomeException) -> do
            _ <- try (dumpFailureBundle tui bundleName) :: IO (Either SomeException FilePath)
            throwIO err

renderFailureBundle ::
    TuiState ->
    Either SomeException FilePath ->
    Either SomeException Text ->
    Maybe ExitCode ->
    Text
renderFailureBundle state snapshotResult viewResult exitResult =
    T.unlines
        [ "tuispec failure bundle"
        , ""
        , "Launched app: " <> T.pack (show (launchedApp state))
        , "Process exit: " <> maybe "(still running or no app)" (T.pack . show) exitResult
        , "Snapshot: " <> renderSnapshotResult snapshotResult
        , ""
        , "Actions:"
        , renderList (reverse (actionLog state))
        , ""
        , "Warnings:"
        , renderList (reverse (runtimeWarnings state))
        , ""
        , "Snapshot artifacts:"
        , renderList (reverse (snapshotLog state))
        , ""
        , "Viewport:"
        , renderViewResult viewResult
        ]
  where
    renderSnapshotResult =
        either
            (("failed: " <>) . T.pack . displayException)
            T.pack

    renderViewResult =
        either
            (("failed: " <>) . T.pack . displayException)
            id

    renderList [] = "  (none)"
    renderList values = T.unlines (map ("  - " <>) values)

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
        createDirectoryIfMissing True testRoot
        tui <- mkTui projectRoot options (specName specDef) testRoot snapshotRoot
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
                case runResult of
                    Left (_ :: SomeException) -> do
                        _ <- try (dumpFailureBundle tui "failure") :: IO (Either SomeException FilePath)
                        pure ()
                    Right () -> pure ()
                activePty <- readPty tui
                case activePty of
                    Just _ -> do
                        _ <- timeout (500 * 1000) (syncVisibleBuffer tui)
                        pure ()
                    Nothing -> pure ()
                case recordTraceTo options of
                    Just tracePath -> do
                        _ <- try (writeRecording tui tracePath) :: IO (Either SomeException FilePath)
                        pure ()
                    Nothing -> pure ()
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

mkTui :: FilePath -> RunOptions -> String -> FilePath -> FilePath -> IO Tui
mkTui projectRoot options name testRoot snapshotRoot = do
    ptyRef <- newIORef Nothing
    sessionStart <- getCurrentTime
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
                , tuiSessionStart = sessionStart
                }
    pure tui

initializePty :: Int -> Int -> App -> IO (Either SomeException PtyHandle)
initializePty rows cols appSpec =
    try (startPty rows cols appSpec)

startPty :: Int -> Int -> App -> IO PtyHandle
startPty rows cols appSpec = do
    case appSpec of
        App{} ->
            startCommandPty rows cols appSpec
        HaskellApp{} ->
            startHaskellPty rows cols appSpec

startCommandPty :: Int -> Int -> App -> IO PtyHandle
startCommandPty rows cols appSpec = do
    processEnv <- withTerminalEnv (env appSpec)
    let (effectiveCommand, effectiveArgs) = wrapLaunchWithCwd appSpec
    (master, processHandle) <-
        Pty.spawnWithPty
            (Just processEnv)
            True
            effectiveCommand
            effectiveArgs
            (cols, rows)
    pure
        PtyHandle
            { ptyMaster = master
            , ptyProcess = processHandle
            }

startHaskellPty :: Int -> Int -> App -> IO PtyHandle
startHaskellPty rows cols appSpec@HaskellApp{} = do
    processEnv <- withTerminalEnv (env appSpec)
    (masterFd, initialSlaveFd) <- openPseudoTerminal
    enablePacketMode masterFd
    maybeMaster <- Pty.createPty masterFd
    master <-
        case maybeMaster of
            Just pty -> pure pty
            Nothing -> throwIO (AssertionError "opened PTY master is not a terminal")
    Pty.resizePty master (cols, rows)
    slaveName <- getSlaveTerminalName masterFd
    processId <-
        forkProcess
            ( runHaskellPtyChild
                processEnv
                initialSlaveFd
                masterFd
                slaveName
                (cwd appSpec)
                (appAction appSpec)
            )
    ignoreIOError (closeFd initialSlaveFd)
    processHandle <- mkProcessHandle (fromIntegral processId) True
    pure
        PtyHandle
            { ptyMaster = master
            , ptyProcess = processHandle
            }
startHaskellPty _ _ App{} =
    throwIO (AssertionError "internal error: startHaskellPty called for command app")

runHaskellPtyChild :: [(String, String)] -> Fd -> Fd -> FilePath -> Maybe FilePath -> IO () -> IO ()
runHaskellPtyChild processEnv initialSlaveFd masterFd slaveName maybeCwd action = do
    result <- try childMain
    case result of
        Right () ->
            exitWith ExitSuccess
        Left (err :: SomeException)
            | Just exitCode <- fromException err ->
                exitWith exitCode
            | otherwise -> do
                hPutStrLn stderr ("tuispec Haskell app failed: " <> displayException err)
                exitImmediately (ExitFailure 1)
  where
    childMain = do
        ignoreIOError (closeFd masterFd)
        ignoreIOError (closeFd initialSlaveFd)
        _ <- createSession
        slaveFd <- openFd slaveName ReadWrite defaultFileFlags
        _ <- dupTo slaveFd stdInput
        _ <- dupTo slaveFd stdOutput
        _ <- dupTo slaveFd stdError
        unless (slaveFd `elem` [stdInput, stdOutput, stdError]) $
            ignoreIOError (closeFd slaveFd)
        PosixEnv.setEnvironment processEnv
        maybe (pure ()) changeWorkingDirectory maybeCwd
        action

{- | Enable TIOCPKT on the master fd. Empirically required to keep the forked
Haskell child process alive on the @HaskellApp@ path; without it the child
exits before the test body can interact with it. The packet-mode prefix byte
is always @< ' '@ and is dropped by the ANSI emulator's printable-char filter.
-}
enablePacketMode :: Fd -> IO ()
enablePacketMode fd =
    throwErrnoIfMinus1_ "failed to enable PTY packet mode" (c_tuispec_enable_packet_mode fd)

withTerminalEnv :: Maybe [(String, Maybe String)] -> IO [(String, String)]
withTerminalEnv envOverrides = do
    existing <- getEnvironment
    let merged =
            case envOverrides of
                Nothing -> existing
                Just overrides ->
                    foldl'
                        (\acc (key, value) -> applyEnvOverride key value acc)
                        existing
                        overrides
    pure
        ( applyEnvOverride "COLORTERM" Nothing
            . applyEnvOverride "TERM" (Just "xterm-256color")
            $ merged
        )
  where
    applyEnvOverride key maybeValue pairs =
        case maybeValue of
            Nothing ->
                filter ((/= key) . fst) pairs
            Just value ->
                (key, value) : filter ((/= key) . fst) pairs

wrapLaunchWithCwd :: App -> (FilePath, [String])
wrapLaunchWithCwd appSpec =
    case cwd appSpec of
        Nothing -> (command appSpec, args appSpec)
        Just cwdPath ->
            ( "/bin/sh"
            ,
                [ "-lc"
                , "cd "
                    <> shellQuote cwdPath
                    <> " && exec "
                    <> unwords (map shellQuote (command appSpec : args appSpec))
                ]
            )

shellQuote :: String -> String
shellQuote value =
    "'" <> concatMap escapeChar value <> "'"
  where
    escapeChar '\'' = "'\\''"
    escapeChar c = [c]

appLabel :: App -> Text
appLabel appSpec =
    case appSpec of
        App{command} -> T.pack command
        HaskellApp{appName} -> T.pack appName

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
    exitedAfterTerminate <- waitForProcessExitWithin (500 * 1000) (ptyProcess ptyHandle)
    unless exitedAfterTerminate $ do
        killPtyProcessGroupNow ptyHandle
        _ <- waitForProcessExitWithin (500 * 1000) (ptyProcess ptyHandle)
        pure ()
    _ <- timeout (500 * 1000) (ignoreIOError (Pty.closePty (ptyMaster ptyHandle)))
    pure ()

waitForProcessExitWithin :: Int -> ProcessHandle -> IO Bool
waitForProcessExitWithin timeoutMicros processHandle =
    go (max 1 (timeoutMicros `div` pollMicros))
  where
    pollMicros = 20 * 1000

    go 0 = isJust <$> getProcessExitCode processHandle
    go remaining = do
        status <- getProcessExitCode processHandle
        case status of
            Just _ ->
                pure True
            Nothing -> do
                threadDelay pollMicros
                go (remaining - 1)

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

failIfLaunchedProcessExited :: Tui -> String -> IO ()
failIfLaunchedProcessExited tui context = do
    exitStatus <- launchedExitStatus tui
    case exitStatus of
        Nothing -> pure ()
        Just exitCode ->
            throwIO
                ( AssertionError
                    ( "launched app exited during "
                        <> context
                        <> ": "
                        <> show exitCode
                    )
                )

launchedExitStatus :: Tui -> IO (Maybe ExitCode)
launchedExitStatus tui = do
    maybePty <- readPty tui
    case maybePty of
        Nothing -> pure Nothing
        Just ptyHandle -> getProcessExitCode (ptyProcess ptyHandle)

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
        state{actionLog = actionText : actionLog state}

appendSnapshotArtifact :: Tui -> FilePath -> IO ()
appendSnapshotArtifact tui snapshotPath = do
    let relativePath = T.pack (makeRelative (tuiRootDir tui) snapshotPath)
    modifyState tui $ \state ->
        if relativePath `elem` snapshotLog state
            then state
            else state{snapshotLog = relativePath : snapshotLog state}

appendWarning :: Tui -> Text -> IO ()
appendWarning tui warningText =
    modifyState tui $ \state ->
        state{runtimeWarnings = warningText : runtimeWarnings state}

recordFrame :: Tui -> Text -> IO ()
recordFrame tui frameText = do
    now <- getCurrentTime
    let elapsedMicros =
            round (realToFrac (diffUTCTime now (tuiSessionStart tui)) * 1e6 :: Double) ::
                Int64
    modifyState tui $ \state ->
        case frameLog state of
            (_, latest) : _ | latest == frameText -> state
            _ -> state{frameLog = (elapsedMicros, frameText) : frameLog state}

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

{- | Encode a mouse click (button press followed by release) as terminal
input bytes.

Takes 1-based column and row. SGR encoding (the default) has no coordinate
limit; X10 encoding offsets each value by 32 and clamps to a single byte, so
it cannot address columns or rows beyond 223.
-}
encodeMouseClick :: ClickOptions -> Int -> Int -> BS.ByteString
encodeMouseClick options col row =
    case clickEncoding options of
        MouseSGR ->
            let sgr final =
                    BS8.pack
                        ( "\ESC[<"
                            <> show buttonCode
                            <> ";"
                            <> show col
                            <> ";"
                            <> show row
                            <> [final]
                        )
             in sgr 'M' <> sgr 'm'
        MouseX10 ->
            let clampByte v = min 255 (max 32 v)
                cx = clampByte (col + 32)
                cy = clampByte (row + 32)
                x10 code =
                    BS.pack
                        [0x1b, 0x5b, 0x4d, fromIntegral (clampByte (code + 32)), fromIntegral cx, fromIntegral cy]
             in x10 buttonCode <> x10 releaseCode
  where
    buttonCode :: Int
    buttonCode =
        case clickButton options of
            MouseLeft -> 0
            MouseMiddle -> 1
            MouseRight -> 2
    releaseCode = 3 :: Int

sendPtyText :: PtyHandle -> Text -> IO ()
sendPtyText ptyHandle textValue =
    Pty.writePty (ptyMaster ptyHandle) (TE.encodeUtf8 textValue)

-- | Write raw bytes to the PTY (used for mouse reports, which are not text).
sendPtyBytes :: PtyHandle -> BS.ByteString -> IO ()
sendPtyBytes ptyHandle = Pty.writePty (ptyMaster ptyHandle)

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

{- | A terminal color, either an exact RGB triple or a palette index (0–15)
that is resolved against the active t'ThemePalette' at render time.
-}
data EmuColor
    = EmuColor Int Int Int
    | EmuPaletteColor Int

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
            , EmuColor 205 49 49
            , EmuColor 0 188 0
            , EmuColor 148 152 0
            , EmuColor 4 81 165
            , EmuColor 188 5 188
            , EmuColor 5 152 188
            , EmuColor 85 85 85
            , EmuColor 102 102 102
            , EmuColor 205 49 49
            , EmuColor 20 158 20
            , EmuColor 158 152 0
            , EmuColor 4 81 165
            , EmuColor 188 5 188
            , EmuColor 5 152 188
            , EmuColor 165 165 165
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

{- | Resolve an t'EmuColor' against the theme palette.

v'EmuPaletteColor' indices are looked up in the theme's 16-color table;
literal v'EmuColor' RGB triples pass through unchanged.
-}
resolveColor :: ThemePalette -> EmuColor -> EmuColor
resolveColor _ (EmuColor r g b) = EmuColor r g b
resolveColor palette (EmuPaletteColor code) =
    fromMaybe (EmuColor 0 0 0) (safeIndex code (paletteAnsi16 palette))

resolveCellFg :: SnapshotTheme -> EmuCellStyle -> EmuColor
resolveCellFg theme styleValue =
    if cellReverse styleValue
        then maybe (paletteDefaultBg palette) (resolveColor palette) (cellBg styleValue)
        else maybe (paletteDefaultFg palette) (resolveColor palette) (cellFg styleValue)
  where
    palette = themePalette theme

resolveCellBg :: SnapshotTheme -> EmuCellStyle -> EmuColor
resolveCellBg theme styleValue =
    if cellReverse styleValue
        then maybe (paletteDefaultFg palette) (resolveColor palette) (cellFg styleValue)
        else maybe (paletteDefaultBg palette) (resolveColor palette) (cellBg styleValue)
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
            | code >= 0 && code < 16 = EmuPaletteColor code
            | otherwise = EmuColor 0 0 0

        colorFromCode value
            | value >= 0 && value < 16 = EmuPaletteColor value
            | otherwise =
                let (r, g, b) = ansiColorFromCode value
                 in EmuColor r g b

{- | Convert a 256-color code (16–255) to an RGB triple.

Palette colors 0–15 are theme-dependent and handled separately as
'EmuPaletteColor' indices.
-}
ansiColorFromCode :: Int -> (Int, Int, Int)
ansiColorFromCode value
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
        case colorValue of
            EmuColor rValue gValue bValue ->
                "[" <> intercalate "," (map show [rValue, gValue, bValue]) <> "]"
            EmuPaletteColor _ ->
                -- Palette colors should already be resolved before serialization.
                "[0,0,0]"

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

assertNotAmbiguousWithMode :: String -> AmbiguityMode -> Selector -> Viewport -> IO ()
assertNotAmbiguousWithMode testName mode selector viewport =
    when shouldFail $
        throwIO $
            AssertionError
                ( "Ambiguous selector for test '"
                    <> testName
                    <> "'; matched "
                    <> show totalMatches
                    <> " elements."
                )
  where
    totalMatches = matchCount selector viewport
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

{- | Resolve a selector to the 0-based @(col, row)@ origin of its first match
in the viewport, or 'Nothing' when nothing matches.

Used by 'clickSelector'. Origins are 0-based to match the 'At' selector.
'Regex' resolves to the first non-blank column of the first matching line.

Matching is line-oriented: a match that spans a line boundary (a multi-line
'Exact' needle, or a 'Regex' whose @.*@ crosses a newline) resolves to
'Nothing' even though the whole-text matchers treat it as a hit.
'clickSelector' turns that into a \"no match\" error.
-}
selectorOrigin :: Selector -> Viewport -> Maybe (Int, Int)
selectorOrigin selector viewport =
    case selector of
        Exact textValue ->
            listToMaybe (exactOrigins textValue (viewportText viewport))
        At col row -> Just (col, row)
        Within rect nested -> do
            (col, row) <-
                selectorOrigin
                    nested
                    (viewport{viewportText = cropRect rect (viewportText viewport)})
            pure (col + rectCol rect, row + rectRow rect)
        Nth idx nested ->
            case nested of
                Exact textValue ->
                    safeIndex idx (exactOrigins textValue (viewportText viewport))
                Regex patternText ->
                    safeIndex idx (regexOrigins patternText (viewportText viewport))
                _ ->
                    if matchCount nested viewport > idx
                        then selectorOrigin nested viewport
                        else Nothing
        Regex patternText ->
            listToMaybe (regexOrigins patternText (viewportText viewport))

{- | All 0-based @(col, row)@ origins of a lightweight regex pattern, one per
matching line, at the column where the pattern's first literal segment matches
(so a selector click lands on the matched text, not the line's left edge).
-}
regexOrigins :: Text -> Text -> [(Int, Int)]
regexOrigins patternText textValue =
    [ (col, rowIdx)
    | (rowIdx, line) <- zip [0 ..] (T.lines textValue)
    , Just col <- [regexLikeMatchOrigin patternText line]
    ]

-- | All 0-based @(col, row)@ origins of a literal substring within viewport text.
exactOrigins :: Text -> Text -> [(Int, Int)]
exactOrigins needle textValue
    | T.null needle = []
    | otherwise =
        [ (col, rowIdx)
        | (rowIdx, line) <- zip [0 ..] (T.lines textValue)
        , col <- lineOccurrences line
        ]
  where
    needleLen = T.length needle
    lineOccurrences = go 0
      where
        go base hay =
            let (prefix, suffix) = T.breakOn needle hay
             in if T.null suffix
                    then []
                    else
                        let col = base + T.length prefix
                         in col : go (col + needleLen) (T.drop (T.length prefix + needleLen) hay)

regexAlternativeCount :: Text -> Text -> Int
regexAlternativeCount alternative haystack
    | ".*" `T.isInfixOf` alternative =
        if wildcardContains alternative haystack then 1 else 0
    | otherwise = occurrenceCount alternative haystack

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

slugify :: String -> String
slugify = safeFileStem

writeSnapshotMetadata :: FilePath -> Int -> Int -> IO ()
writeSnapshotMetadata metaPath rows cols =
    BL.writeFile
        metaPath
        ( encode
            ( object
                [ "rows" .= rows
                , "cols" .= cols
                , "version" .= tuispecVersion
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
            "last" -> Just LastVisibleMatch
            "last-visible" -> Just LastVisibleMatch
            _ -> Nothing
