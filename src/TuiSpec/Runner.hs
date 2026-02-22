{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TuiSpec.Runner
  ( expectNotVisible
  , expectSnapshot
  , expectVisible
  , launch
  , press
  , pressCombo
  , runSuite
  , step
  , test
  , typeText
  , waitFor
  , waitForText
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
  ( Exception
  , SomeException
  , catch
  , displayException
  , finally
  , throwIO
  , try
  )
import Control.Monad (forM, unless, when)
import Data.Char (chr, isAlphaNum, ord, toLower)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (intercalate, isSuffixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory
  ( canonicalizePath
  , copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findExecutable
  , getCurrentDirectory
  , listDirectory
  , removeFile
  , removePathForcibly
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO
  ( BufferMode(..)
  , Handle
  , hClose
  , hFlush
  , hGetChar
  , hReady
  , hSetBuffering
  , openTempFile
  )
import System.Process
  ( CreateProcess(..)
  , StdStream(..)
  , createProcess
  , getProcessExitCode
  , proc
  , readCreateProcessWithExitCode
  , terminateProcess
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
  modifyState tui $ \state -> state {launchedApp = Just app}
  case tuiPty tui of
    Just ptyHandle -> do
      sendPtyText ptyHandle (T.pack (renderAppCommand app) <> "\n")
      threadDelay settleDelayMicros
      _ <- syncVisibleBuffer tui
      pure ()
    Nothing ->
      throwIO (AssertionError "PTY backend unavailable during launch")

press :: Tui -> Key -> IO ()
press tui key = do
  appendAction tui ("press " <> renderKey key)
  case tuiPty tui of
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
  case tuiPty tui of
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
  case tuiPty tui of
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
  renderSnapshotPng actualPngPath normalized

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
      (do
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
  maybePty <- initializePty
  ptyHandle <-
    case maybePty of
      Just handle ->
        pure handle
      Nothing ->
        throwIO (AssertionError "PTY backend unavailable. Install 'script' and ensure PTY access.")
  tui <-
    pure
      Tui
        { tuiName = name
        , tuiOptions = options
        , tuiTestRoot = testRoot
        , tuiSnapshotRoot = snapshotRoot
        , tuiPty = Just ptyHandle
        , tuiStateRef = stateRef
        }
  _ <- syncVisibleBuffer tui
  pure tui

initializePty :: IO (Maybe PtyHandle)
initializePty = do
  hasScript <- findExecutable "script"
  case hasScript of
    Nothing -> pure Nothing
    Just _ -> do
      result <- try startPty :: IO (Either SomeException PtyHandle)
      case result of
        Left _ -> pure Nothing
        Right handle -> pure (Just handle)

startPty :: IO PtyHandle
startPty =
  tryStartScript
    [ ["-q", "-f", "/dev/null", "sh"]
    , ["-q", "-F", "/dev/null", "sh"]
    , ["-q", "/dev/null", "sh"]
    ]
 where
  tryStartScript [] =
    throwIO (AssertionError "failed to start script PTY helper with supported flags")
  tryStartScript (scriptArgs : rest) = do
    handle <- spawnScript scriptArgs
    threadDelay (50 * 1000)
    exited <- getProcessExitCode (ptyProcess handle)
    case exited of
      Nothing -> pure handle
      Just _ -> do
        ignoreIOError (hClose (ptyIn handle))
        ignoreIOError (hClose (ptyOut handle))
        tryStartScript rest

  spawnScript scriptArgs = do
    let cp =
          (proc "script" scriptArgs)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    (maybeIn, maybeOut, maybeErr, processHandle) <- createProcess cp
    inputHandle <-
      case maybeIn of
        Just value -> pure value
        Nothing -> throwIO (AssertionError "pty input handle was not created")
    outputHandle <-
      case maybeOut of
        Just value -> pure value
        Nothing -> throwIO (AssertionError "pty output handle was not created")
    case maybeErr of
      Just errHandle -> ignoreIOError (hClose errHandle)
      Nothing -> pure ()
    hSetBuffering inputHandle NoBuffering
    hSetBuffering outputHandle NoBuffering
    pure
      PtyHandle
        { ptyIn = inputHandle
        , ptyOut = outputHandle
        , ptyProcess = processHandle
        }

teardownTui :: Tui -> IO ()
teardownTui tui =
  case tuiPty tui of
    Just ptyHandle -> do
      ignoreIOError (sendPtyText ptyHandle "exit\n")
      ignoreIOError (terminateProcess (ptyProcess ptyHandle))
      ignoreIOError (hClose (ptyIn ptyHandle))
      ignoreIOError (hClose (ptyOut ptyHandle))
    _ -> pure ()

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
syncVisibleBuffer tui =
  case tuiPty tui of
    Just ptyHandle -> do
      chunk <- try (drainPtyOutput (ptyOut ptyHandle)) :: IO (Either SomeException Text)
      case chunk of
        Left err -> do
          appendWarning tui ("failed to read pty output: " <> T.pack (displayException err))
          visibleBuffer <$> readIORef (tuiStateRef tui)
        Right textChunk -> do
          state <- readIORef (tuiStateRef tui)
          let cleanChunk = normalizeChunk textChunk
          let nextRaw = rawBuffer state <> cleanChunk
          let nextVisible = viewportFromRaw (terminalRows (tuiOptions tui)) nextRaw
          modifyState tui $ \st -> st {rawBuffer = nextRaw, visibleBuffer = nextVisible}
          recordFrame tui nextVisible
          pure nextVisible
    Nothing ->
      throwIO (AssertionError "PTY backend unavailable during viewport sync")

modifyState :: Tui -> (TuiState -> TuiState) -> IO ()
modifyState tui updateFn = modifyIORef' (tuiStateRef tui) updateFn

appendAction :: Tui -> Text -> IO ()
appendAction tui actionText =
  modifyState tui $ \state ->
    state {actionLog = actionLog state <> [actionText]}

appendSnapshotLog :: Tui -> Text -> IO ()
appendSnapshotLog tui snapshotText =
  modifyState tui $ \state ->
    state {snapshotLog = snapshotLog state <> [snapshotText]}

appendWarning :: Tui -> Text -> IO ()
appendWarning tui warningText =
  modifyState tui $ \state ->
    state {runtimeWarnings = runtimeWarnings state <> [warningText]}

recordFrame :: Tui -> Text -> IO ()
recordFrame tui frameText =
  modifyState tui $ \state ->
    case reverse (frameLog state) of
      latest : _ | latest == frameText -> state
      _ -> state {frameLog = frameLog state <> [frameText]}

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
    Enter -> Just "\n"
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
sendPtyText ptyHandle textValue = do
  TIO.hPutStr (ptyIn ptyHandle) textValue
  hFlush (ptyIn ptyHandle)

drainPtyOutput :: Handle -> IO Text
drainPtyOutput outputHandle = T.pack . reverse <$> go []
 where
  go acc = do
    ready <- hReady outputHandle
    if not ready
      then pure acc
      else do
        charValue <- hGetChar outputHandle
        go (charValue : acc)

normalizeChunk :: Text -> Text
normalizeChunk =
  stripAnsi
    . T.replace "\r\n" "\n"
    . T.replace "\r" "\n"

stripAnsi :: Text -> Text
stripAnsi input = T.pack (goNormal (T.unpack input))
 where
  goNormal [] = []
  goNormal ('\ESC' : '[' : rest) = goCsi rest
  goNormal ('\ESC' : _ : rest) = goNormal rest
  goNormal (c : rest)
    | isPrintable c = c : goNormal rest
    | otherwise = goNormal rest

  goCsi [] = []
  goCsi (c : rest)
    | c >= '@' && c <= '~' = goNormal rest
    | otherwise = goCsi rest

  isPrintable c = c == '\n' || c == '\t' || c >= ' '

viewportFromRaw :: Int -> Text -> Text
viewportFromRaw rows rawText =
  T.intercalate "\n" (takeLast rows (T.lines rawText))

takeLast :: Int -> [a] -> [a]
takeLast count values
  | count <= 0 = []
  | otherwise =
      let valueCount = length values
       in drop (max 0 (valueCount - count)) values

renderAppCommand :: App -> String
renderAppCommand app = unwords (map shellQuote (command app : args app))

shellQuote :: String -> String
shellQuote textValue
  | all isSafe textValue = textValue
  | otherwise = "'" <> concatMap escape textValue <> "'"
 where
  isSafe c = isAlphaNum c || c `elem` ("-._/:+=" :: String)
  escape '\'' = "'\"'\"'"
  escape c = [c]

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
      selectorMatches nested (viewport {viewportText = cropRect rect (viewportText viewport)})
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
  createDirectoryIfMissing True (takeDirectory outPath)
  (tmpInPath, tmpHandle) <- openTempFile (takeDirectory outPath) "snapshot-"
  TIO.hPutStr tmpHandle textValue
  hClose tmpHandle
  let args = ["-c", pythonRenderScript, tmpInPath, outPath]
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
    , "with open(inp, 'r', encoding='utf-8', errors='replace') as f:"
    , "    lines = f.read().splitlines()"
    , "font = ImageFont.load_default()"
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
  projectRootOverride <- lookupEnv "TUISPEC_PROJECT_ROOT"
  projectRoot <-
    case projectRootOverride of
      Just override -> canonicalizePath override
      Nothing -> do
        cwd <- getCurrentDirectory
        locateProjectRoot cwd
  canonicalizePath (projectRoot </> "snapshots")

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
