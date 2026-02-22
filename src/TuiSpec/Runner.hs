{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TuiSpec.Runner (
    expectNotVisible,
    expectSnapshot,
    expectVisible,
    launch,
    press,
    pressCombo,
    runSuite,
    step,
    test,
    typeText,
    waitFor,
    waitForText,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (
    Exception,
    SomeException,
    catch,
    displayException,
    finally,
    throwIO,
    try,
 )
import Control.Monad (filterM, forM, unless, when)
import Data.ByteString qualified as BS
import Data.Char (chr, isAlphaNum, ord, toLower)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.List (intercalate, isSuffixOf, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (
    canonicalizePath,
    copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
    removeFile,
    removePathForcibly,
 )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (
    hClose,
    openTempFile,
 )
import System.Posix.Pty qualified as Pty
import System.Process (
    proc,
    readCreateProcessWithExitCode,
    terminateProcess,
 )
import Text.Read (readMaybe)
import TuiSpec.Types

data TestStatus
    = Passed
    | Failed Text
    deriving (Eq, Show)

data TestResult = TestResult
    { resultName :: String
    , resultSlug :: String
    , resultStatus :: TestStatus
    , resultDurationMs :: Int
    , resultAttempts :: Int
    , resultArtifactsPath :: FilePath
    }
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

test :: String -> (Tui -> IO ()) -> Spec
test = Spec

runSuite :: RunOptions -> [Spec] -> IO ExitCode
runSuite options specs = do
    effectiveOptions <- applyEnvOverrides options
    snapshotsBase <- resolveSnapshotsBaseDir
    runRoot <- mkRunRoot effectiveOptions
    results <- forM specs (runOneSpec effectiveOptions snapshotsBase runRoot)
    writeRunJson runRoot results
    writeRunMarkdown runRoot results
    printSummary results
    if any isFailure results
        then pure (ExitFailure 1)
        else pure ExitSuccess
  where
    isFailure result = case resultStatus result of
        Passed -> False
        Failed _ -> True

launch :: Tui -> App -> IO ()
launch tui app = do
    appendAction tui ("launch " <> T.pack (command app))
    currentPty <- readPty tui
    case currentPty of
        Just ptyHandle -> do
            ignoreIOError (sendPtyText ptyHandle "exit\n")
            ignoreIOError (Pty.closePty (ptyMaster ptyHandle))
            ignoreIOError (terminateProcess (ptyProcess ptyHandle))
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

expectVisible :: Tui -> Selector -> IO ()
expectVisible tui selector = do
    waitFor tui defaultWaitOptions (selectorMatches selector)
    viewport <- currentViewport tui
    assertNotAmbiguous tui selector viewport

expectNotVisible :: Tui -> Selector -> IO ()
expectNotVisible tui selector =
    waitFor tui defaultWaitOptions (not . selectorMatches selector)

waitForText :: Tui -> Selector -> IO ()
waitForText tui selector = do
    waitFor tui defaultWaitOptions (selectorMatches selector)
    viewport <- currentViewport tui
    assertNotAmbiguous tui selector viewport

waitFor :: Tui -> WaitOptions -> (Viewport -> Bool) -> IO ()
waitFor tui waitOptions predicate = do
    start <- getCurrentTime
    loop start
  where
    timeout = fromIntegral (timeoutMs waitOptions) / 1000 :: NominalDiffTime

    loop :: UTCTime -> IO ()
    loop startedAt = do
        viewport <- currentViewport tui
        if predicate viewport
            then pure ()
            else do
                now <- getCurrentTime
                if diffUTCTime now startedAt >= timeout
                    then throwIO (AssertionError "waitFor timed out")
                    else do
                        threadDelay (pollIntervalMs waitOptions * 1000)
                        loop startedAt

expectSnapshot :: Tui -> SnapshotName -> IO ()
expectSnapshot tui snapshotName = do
    when (T.null (unSnapshotName snapshotName)) $
        throwIO (AssertionError "snapshot name cannot be empty")
    _ <- syncVisibleBuffer tui
    state <- readIORef (tuiStateRef tui)
    let requestedTheme = snapshotTheme (tuiOptions tui)
        resolvedTheme = resolveSnapshotTheme requestedTheme
    when (not (isKnownSnapshotTheme requestedTheme)) $
        appendWarning
            tui
            ( "Unknown snapshot theme '"
                <> T.pack requestedTheme
                <> "', using "
                <> T.pack (snapshotThemeName defaultSnapshotTheme)
                <> "."
            )
    let emuState =
            emulateAnsi
                (emptyEmuState (terminalRows (tuiOptions tui)) (terminalCols (tuiOptions tui)))
                (T.unpack (rawBuffer state))
    viewport <- currentViewport tui
    let snapshotStem = safeFileStem (T.unpack (unSnapshotName snapshotName))
    let actualDir = tuiTestRoot tui </> "snapshots"
    let baselineDir = tuiSnapshotRoot tui
    createDirectoryIfMissing True actualDir
    createDirectoryIfMissing True baselineDir

    let normalized = normalizeSnapshotText (viewportText viewport)
    let actualTextPath = actualDir </> (snapshotStem <> "-actual.txt")
    let actualPngPath = actualDir </> (snapshotStem <> "-actual.png")
    let baselineTextPath = baselineDir </> (snapshotStem <> ".txt")
    let baselinePngPath = baselineDir </> (snapshotStem <> ".png")

    TIO.writeFile actualTextPath normalized
    renderStyledSnapshotPng actualPngPath resolvedTheme emuState

    baselineExists <- doesFileExist baselineTextPath
    if updateSnapshots (tuiOptions tui) || not baselineExists
        then do
            TIO.writeFile baselineTextPath normalized
            copyFile actualPngPath baselinePngPath
            appendSnapshotLog tui ("snapshot " <> T.pack snapshotStem <> " updated")
        else do
            baselineText <- TIO.readFile baselineTextPath
            let baselineNormalized = normalizeSnapshotText baselineText
            if baselineNormalized == normalized
                then appendSnapshotLog tui ("snapshot " <> T.pack snapshotStem <> " matched")
                else do
                    let expectedTextPath = actualDir </> (snapshotStem <> "-expected.txt")
                    let expectedPngPath = actualDir </> (snapshotStem <> "-expected.png")
                    let diffTextPath = actualDir </> (snapshotStem <> "-diff.txt")
                    let diffPngPath = actualDir </> (snapshotStem <> "-diff.png")
                    TIO.writeFile expectedTextPath baselineNormalized
                    baselinePngExists <- doesFileExist baselinePngPath
                    if baselinePngExists
                        then copyFile baselinePngPath expectedPngPath
                        else renderSnapshotPng expectedPngPath baselineNormalized
                    writeFile diffTextPath (renderTextDiff baselineNormalized normalized)
                    renderTextPng diffPngPath (renderDiffSideBySide baselineNormalized normalized)
                    appendSnapshotLog tui ("snapshot " <> T.pack snapshotStem <> " mismatch")
                    throwIO $
                        AssertionError
                            ( "Snapshot mismatch for '"
                                <> snapshotStem
                                <> "'. See "
                                <> expectedPngPath
                                <> " and "
                                <> actualPngPath
                                <> " (diff: "
                                <> diffPngPath
                                <> ")"
                            )

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

runOneSpec :: RunOptions -> FilePath -> FilePath -> Spec -> IO TestResult
runOneSpec options snapshotsBase runRoot specDef = do
    start <- getCurrentTime
    let slug = slugify (specName specDef)
    let testRoot = runRoot </> "tests" </> slug </> "attempt-final"
    let snapshotRoot = snapshotsBase </> slug
    (status, finalState, attempts) <- executeWithRetries options specDef testRoot snapshotRoot
    end <- getCurrentTime
    emitTestArtifacts testRoot options finalState status
    pure
        TestResult
            { resultName = specName specDef
            , resultSlug = slug
            , resultStatus = status
            , resultDurationMs = floor (diffUTCTime end start * 1000)
            , resultAttempts = attempts
            , resultArtifactsPath = "tests" </> slug </> "attempt-final"
            }

executeWithRetries :: RunOptions -> Spec -> FilePath -> FilePath -> IO (TestStatus, TuiState, Int)
executeWithRetries options specDef testRoot snapshotRoot = go 1
  where
    maxAttempts = max 1 (retries options + 1)

    go attempt = do
        resetDirectory testRoot
        tui <- mkTui options (specName specDef) testRoot snapshotRoot attempt
        runResultAndState <-
            ( do
                runResult <- try (specBody specDef tui)
                _ <- syncVisibleBuffer tui
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

mkTui :: RunOptions -> String -> FilePath -> FilePath -> Int -> IO Tui
mkTui options name testRoot snapshotRoot _attempt = do
    createDirectoryIfMissing True testRoot
    createDirectoryIfMissing True snapshotRoot
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
            Just ptyHandle -> do
                ignoreIOError (sendPtyText ptyHandle "exit\n")
                ignoreIOError (Pty.closePty (ptyMaster ptyHandle))
                ignoreIOError (terminateProcess (ptyProcess ptyHandle))
            _ -> pure ()
        writePty tui Nothing

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
            outChunk <- try (drainPtyOutput (ptyMaster ptyHandle)) :: IO (Either SomeException Text)
            combinedChunk <-
                case outChunk of
                    Left outErr -> do
                        appendWarning tui ("failed to read pty output: " <> T.pack (displayException outErr))
                        pure ""
                    Right outText ->
                        pure outText
            case combinedChunk of
                "" ->
                    visibleBuffer <$> readIORef (tuiStateRef tui)
                textChunk -> do
                    state <- readIORef (tuiStateRef tui)
                    let nextRaw = rawBuffer state <> textChunk
                    let nextVisible =
                            viewportFromAnsiRaw
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

appendSnapshotLog :: Tui -> Text -> IO ()
appendSnapshotLog tui snapshotText =
    modifyState tui $ \state ->
        state{snapshotLog = snapshotLog state <> [snapshotText]}

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
            ([AltModifier], CharKey c) ->
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

drainPtyOutput :: Pty.Pty -> IO Text
drainPtyOutput pty = T.concat . reverse <$> go []
  where
    go acc = do
        next <- Pty.tryReadPty pty
        case next of
            Right chunk
                | not (BS.null chunk) ->
                    go (TE.decodeUtf8With TEE.lenientDecode chunk : acc)
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

viewportFromAnsiRaw :: Int -> Int -> Text -> Text
viewportFromAnsiRaw rows cols rawText =
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
                if modeValue == 2 || modeValue == 3 || modeValue == 0
                    then clearScreen stateValue
                    else stateValue
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

normalizeSnapshotText :: Text -> Text
normalizeSnapshotText =
    T.intercalate "\n"
        . reverse
        . dropWhile T.null
        . reverse
        . map normalizeLine
        . T.lines
        . T.replace "\r" ""
  where
    normalizeLine = T.dropWhileEnd (\c -> c == ' ' || c == '\t')

renderDiffSideBySide :: Text -> Text -> Text
renderDiffSideBySide expected actual =
    T.unlines
        [ "=== EXPECTED ==="
        , expected
        , ""
        , "=== ACTUAL ==="
        , actual
        ]

renderSnapshotPng :: FilePath -> Text -> IO ()
renderSnapshotPng = renderTextPng

renderStyledSnapshotPng :: FilePath -> SnapshotTheme -> EmuState -> IO ()
renderStyledSnapshotPng outPath theme emuState = do
    maybeFontPath <- resolveFontPath
    let fontArg = fromMaybe "" maybeFontPath
    createDirectoryIfMissing True (takeDirectory outPath)
    (tmpInPath, tmpHandle) <- openTempFile (takeDirectory outPath) "snapshot-styled-"
    TIO.hPutStr tmpHandle (T.pack (serializeSnapshot theme emuState))
    hClose tmpHandle
    let args =
            ["-c", pythonStyledRenderScript, tmpInPath, outPath, fontArg]
    result <- readCreateProcessWithExitCode (proc "python3" args) ""
    ignoreIOError (removeFile tmpInPath)
    case result of
        (ExitSuccess, _, _) -> pure ()
        (_, _, stderrText) ->
            throwIO $
                AssertionError
                    ( "Failed to render styled PNG (python3 + Pillow required in PATH). "
                        <> stderrText
                    )

renderTextDiff :: Text -> Text -> String
renderTextDiff expected actual =
    unlines $
        ["--- expected", "+++ actual"]
            <> go 1 (T.lines expected) (T.lines actual)
  where
    go :: Int -> [Text] -> [Text] -> [String]
    go _ [] [] = []
    go lineNo (e : es) [] =
        ("@@ line " <> show lineNo <> " @@")
            : ("- " <> T.unpack e)
            : ("+ ")
            : go (lineNo + 1) es []
    go lineNo [] (a : as) =
        ("@@ line " <> show lineNo <> " @@")
            : ("- ")
            : ("+ " <> T.unpack a)
            : go (lineNo + 1) [] as
    go lineNo (e : es) (a : as)
        | e == a = go (lineNo + 1) es as
        | otherwise =
            ("@@ line " <> show lineNo <> " @@")
                : ("- " <> T.unpack e)
                : ("+ " <> T.unpack a)
                : go (lineNo + 1) es as

renderTextPng :: FilePath -> Text -> IO ()
renderTextPng outPath textValue = do
    maybeFontPath <- resolveFontPath
    let fontArg = fromMaybe "" maybeFontPath
    createDirectoryIfMissing True (takeDirectory outPath)
    (tmpInPath, tmpHandle) <- openTempFile (takeDirectory outPath) "snapshot-"
    TIO.hPutStr tmpHandle textValue
    hClose tmpHandle
    let args =
            ["-c", pythonRenderScript, tmpInPath, outPath, fontArg]
    result <- readCreateProcessWithExitCode (proc "python3" args) ""
    ignoreIOError (removeFile tmpInPath)
    case result of
        (ExitSuccess, _, _) -> pure ()
        (_, _, stderrText) ->
            throwIO $
                AssertionError
                    ( "Failed to render PNG (python3 + Pillow required in PATH). "
                        <> stderrText
                    )

pythonRenderScript :: String
pythonRenderScript =
    unlines
        [ "import sys"
        , "from PIL import Image, ImageDraw, ImageFont"
        , "inp, out = sys.argv[1], sys.argv[2]"
        , "font_path = sys.argv[3] if len(sys.argv) > 3 else ''"
        , "font_candidates = []"
        , "if font_path:"
        , "    font_candidates.append(font_path)"
        , "font_candidates += ["
        , "    './fonts/IosevkaMono-Regular.ttc',"
        , "    './fonts/Iosevka-Regular.ttc',"
        , "    './fonts/IosevkaMono-Regular.ttf',"
        , "    './fonts/Iosevka-Regular.ttf',"
        , "    '/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf',"
        , "    '/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf'"
        , " ]"
        , "font = None"
        , "for candidate in font_candidates:"
        , "    if not candidate:"
        , "        continue"
        , "    try:"
        , "        font = ImageFont.truetype(candidate, 14)"
        , "        break"
        , "    except OSError:"
        , "        continue"
        , "if font is None:"
        , "    font = ImageFont.load_default()"
        , "with open(inp, 'r', encoding='utf-8', errors='replace') as f:"
        , "    lines = f.read().splitlines()"
        , "probe = font.getbbox('Hg')"
        , "line_h = (probe[3] - probe[1]) + 4"
        , "max_w = 0"
        , "for line in lines:"
        , "    max_w = max(max_w, font.getbbox(line)[2])"
        , "width = max(160, max_w + 20)"
        , "height = max(40, max(1, len(lines)) * line_h + 20)"
        , "img = Image.new('RGB', (width, height), 'white')"
        , "draw = ImageDraw.Draw(img)"
        , "y = 10"
        , "for line in lines:"
        , "    draw.text((10, y), line, fill='black', font=font)"
        , "    y += line_h"
        , "img.save(out, 'PNG')"
        ]

pythonStyledRenderScript :: String
pythonStyledRenderScript =
    unlines
        [ "import json"
        , "import sys"
        , "from PIL import Image, ImageDraw, ImageFont"
        , "inp, out = sys.argv[1], sys.argv[2]"
        , "font_path = sys.argv[3] if len(sys.argv) > 3 else ''"
        , "font_candidates = []"
        , "if font_path:"
        , "    font_candidates.append(font_path)"
        , "font_candidates += ["
        , "    './fonts/IosevkaMono-Regular.ttc',"
        , "    './fonts/Iosevka-Regular.ttc',"
        , "    './fonts/IosevkaMono-Regular.ttf',"
        , "    './fonts/Iosevka-Regular.ttf',"
        , "    '/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf',"
        , "    '/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf'"
        , " ]"
        , "font = None"
        , "for candidate in font_candidates:"
        , "    if not candidate:"
        , "        continue"
        , "    try:"
        , "        font = ImageFont.truetype(candidate, 12)"
        , "        break"
        , "    except OSError:"
        , "        continue"
        , "if font is None:"
        , "    font = ImageFont.load_default()"
        , "with open(inp, 'r', encoding='utf-8', errors='replace') as f:"
        , "    payload = json.load(f)"
        , "rows = payload['rows']"
        , "cols = payload['cols']"
        , "cells = payload['cells']"
        , "default_bg = tuple(payload.get('defaultBg', (13, 17, 23)))"
        , "probe = font.getbbox('Hg')"
        , "line_h = max(1, (probe[3] - probe[1]) + 2)"
        , "cell_w = max(1, (font.getbbox('W')[2] - font.getbbox('W')[0]))"
        , "padding_x = 1"
        , "padding_y = 1"
        , "width = max(1, cols * cell_w + (padding_x * 2))"
        , "height = max(1, rows * line_h + (padding_y * 2))"
        , "img = Image.new('RGB', (width, height), default_bg)"
        , "draw = ImageDraw.Draw(img)"
        , "y = padding_y"
        , "for row_idx in range(rows):"
        , "    x = padding_x"
        , "    row = cells[row_idx] if row_idx < len(cells) else []"
        , "    for col_idx in range(cols):"
        , "        cell = row[col_idx] if col_idx < len(row) else [32, payload.get('defaultFg', [229, 229, 229]), list(default_bg)]"
        , "        if len(cell) != 3:"
        , "            continue"
        , "        ch = chr(cell[0])"
        , "        fg = tuple(cell[1])"
        , "        bg = tuple(cell[2])"
        , "        rect = (x, y, x + cell_w, y + line_h)"
        , "        draw.rectangle(rect, fill=bg)"
        , "        draw.text((x, y), ch, fill=fg, font=font)"
        , "        x += cell_w"
        , "    y += line_h"
        , "img.save(out, 'PNG')"
        ]

emitTestArtifacts :: FilePath -> RunOptions -> TuiState -> TestStatus -> IO ()
emitTestArtifacts testRoot _options state status = do
    createDirectoryIfMissing True testRoot
    TIO.writeFile (testRoot </> "viewport.txt") (visibleBuffer state)
    writeFile (testRoot </> "steps.json") (renderStepsJson state status)
    unless (launchedApp state == Nothing) $
        writeFile (testRoot </> "launch.txt") (show (launchedApp state))

renderStepsJson :: TuiState -> TestStatus -> String
renderStepsJson state status =
    "{\n"
        <> "  \"status\": "
        <> quoted (statusText status)
        <> ",\n"
        <> "  \"steps\": [\n"
        <> intercalate ",\n" (map renderItem (zip [1 :: Int ..] (actionLog state)))
        <> "\n  ],\n"
        <> "  \"snapshots\": [\n"
        <> intercalate ",\n" (map renderTextItem (snapshotLog state))
        <> "\n  ],\n"
        <> "  \"warnings\": [\n"
        <> intercalate ",\n" (map renderTextItem (runtimeWarnings state))
        <> "\n  ]\n"
        <> "}\n"
  where
    renderItem (idx, actionText) =
        "    {\"id\": "
            <> show idx
            <> ", \"action\": "
            <> quoted (T.unpack actionText)
            <> "}"

    renderTextItem value = "    " <> quoted (T.unpack value)

    statusText Passed = "passed"
    statusText (Failed _) = "failed"

writeRunJson :: FilePath -> [TestResult] -> IO ()
writeRunJson runRoot results =
    writeFile (runRoot </> "report.json") $
        "{\n"
            <> "  \"tests\": [\n"
            <> intercalate ",\n" (map renderResult results)
            <> "\n  ]\n"
            <> "}\n"
  where
    renderResult result =
        "    {"
            <> "\"name\": "
            <> quoted (resultName result)
            <> ", \"slug\": "
            <> quoted (resultSlug result)
            <> ", \"status\": "
            <> quoted (statusText (resultStatus result))
            <> ", \"durationMs\": "
            <> show (resultDurationMs result)
            <> ", \"attempts\": "
            <> show (resultAttempts result)
            <> ", \"artifacts\": "
            <> quoted (resultArtifactsPath result)
            <> maybeErrorField (resultStatus result)
            <> "}"

    maybeErrorField Passed = ""
    maybeErrorField (Failed err) = ", \"error\": " <> quoted (T.unpack err)

    statusText Passed = "passed"
    statusText (Failed _) = "failed"

writeRunMarkdown :: FilePath -> [TestResult] -> IO ()
writeRunMarkdown runRoot results = do
    rows <- forM results (row runRoot)
    writeFile (runRoot </> "report.md") $
        "# tuispec run report\n\n"
            <> "| Test | Status | Attempts | Duration (ms) | Artifacts |\n"
            <> "|---|---|---:|---:|---|\n"
            <> concat rows
  where
    row root result = do
        links <- artifactLinks root (resultArtifactsPath result)
        pure $
            "| "
                <> resultName result
                <> " | "
                <> statusText (resultStatus result)
                <> " | "
                <> show (resultAttempts result)
                <> " | "
                <> show (resultDurationMs result)
                <> " | "
                <> links
                <> " |\n"

    artifactLinks root testArtifactPath = do
        snapshotNames <- listSnapshotEntries (root </> testArtifactPath </> "snapshots")
        let baseLinks =
                [ "[viewport](" <> testArtifactPath <> "/viewport.txt)"
                , "[steps](" <> testArtifactPath <> "/steps.json)"
                , "[snapshots](" <> testArtifactPath <> "/snapshots)"
                ]
        let snapshotLinks =
                [ "[" <> fileName <> "](" <> testArtifactPath <> "/snapshots/" <> fileName <> ")"
                | fileName <- snapshotNames
                ]
        pure (intercalate ", " (baseLinks <> snapshotLinks))

    statusText Passed = "passed"
    statusText (Failed _) = "failed"

printSummary :: [TestResult] -> IO ()
printSummary results = do
    let failed = filter isFailure results
    putStrLn ("Ran " <> show (length results) <> " test(s).")
    if null failed
        then putStrLn "All tests passed."
        else do
            putStrLn (show (length failed) <> " test(s) failed:")
            mapM_ printFailure failed
  where
    isFailure result = case resultStatus result of
        Passed -> False
        Failed _ -> True

    printFailure result =
        case resultStatus result of
            Passed -> pure ()
            Failed err ->
                putStrLn
                    ( "  - "
                        <> resultName result
                        <> ": "
                        <> T.unpack err
                    )

resolveSnapshotsBaseDir :: IO FilePath
resolveSnapshotsBaseDir = do
    projectRoot <- resolveProjectRoot
    canonicalizePath (projectRoot </> "snapshots")

resolveProjectRoot :: IO FilePath
resolveProjectRoot = do
    projectRootOverride <- lookupEnv "TUISPEC_PROJECT_ROOT"
    case projectRootOverride of
        Just override -> canonicalizePath override
        Nothing -> do
            cwd <- getCurrentDirectory
            locateProjectRoot cwd

resolveFontPath :: IO (Maybe FilePath)
resolveFontPath = do
    projectRoot <- resolveProjectRoot
    envFont <- lookupEnv "TUISPEC_FONT_PATH"
    let candidateFonts =
            maybe [] (\p -> [p, projectRoot </> p]) envFont
                <> [ projectRoot </> "fonts" </> "IosevkaMono-Regular.ttc"
                   , projectRoot </> "fonts" </> "Iosevka-Regular.ttc"
                   , projectRoot </> "fonts" </> "IosevkaMono-Regular.ttf"
                   , projectRoot </> "fonts" </> "Iosevka-Regular.ttf"
                   ]
    existing <- filterM doesFileExist candidateFonts
    pure (listToMaybe existing)

locateProjectRoot :: FilePath -> IO FilePath
locateProjectRoot startDir = do
    absoluteStart <- canonicalizePath startDir
    go absoluteStart absoluteStart
  where
    go startRoot currentDir = do
        markerPresent <- hasProjectMarker currentDir
        if markerPresent
            then pure currentDir
            else do
                let parentDir = takeDirectory currentDir
                if parentDir == currentDir
                    then pure startRoot
                    else go startRoot parentDir

hasProjectMarker :: FilePath -> IO Bool
hasProjectMarker dir = do
    hasGit <- doesDirectoryExist (dir </> ".git")
    hasCabalProject <- doesFileExist (dir </> "cabal.project")
    entries <- listDirectory dir `catch` \(_ :: SomeException) -> pure []
    let hasCabalFile = any (".cabal" `isSuffixOf`) entries
    pure (hasGit || hasCabalProject || hasCabalFile)

mkRunRoot :: RunOptions -> IO FilePath
mkRunRoot options = do
    createDirectoryIfMissing True (artifactsDir options)
    now <- getCurrentTime
    let stamp = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%SZ" now
    let runRoot = artifactsDir options </> ("run-" <> stamp)
    createDirectoryIfMissing True runRoot
    pure runRoot

resetDirectory :: FilePath -> IO ()
resetDirectory dir = do
    exists <- doesDirectoryExist dir
    when exists (removePathForcibly dir)
    createDirectoryIfMissing True dir

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

listSnapshotEntries :: FilePath -> IO [FilePath]
listSnapshotEntries snapshotDir = do
    exists <- doesDirectoryExist snapshotDir
    if not exists
        then pure []
        else do
            entries <- listDirectory snapshotDir
            pure (sort (filter isSnapshotArtifact entries))
  where
    isSnapshotArtifact fileName =
        ".png" `isSuffixOf` fileName || ".txt" `isSuffixOf` fileName

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

quoted :: String -> String
quoted value = "\"" <> concatMap escape value <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c = [c]
