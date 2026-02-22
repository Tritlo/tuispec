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
import Control.Exception (Exception, SomeException, displayException, throwIO, try)
import Control.Monad (forM, unless, when)
import Data.Char (isAlphaNum, toLower)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
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

test :: String -> (Tui -> IO ()) -> Spec
test = Spec

runSuite :: RunOptions -> [Spec] -> IO ExitCode
runSuite options specs = do
  runRoot <- mkRunRoot options
  results <- forM specs (runOneSpec options runRoot)
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
  modifyState tui $ \state ->
    state
      { launchedApp = Just app
      , actionLog = actionLog state <> ["launch " <> T.pack (command app)]
      }

press :: Tui -> Key -> IO ()
press tui key = do
  let appended =
        case key of
          Enter -> "\n"
          CharKey c -> T.singleton c
          _ -> ""
  modifyState tui $ \state ->
    state
      { visibleBuffer = visibleBuffer state <> appended
      , actionLog = actionLog state <> ["press " <> renderKey key]
      }

pressCombo :: Tui -> [Modifier] -> Key -> IO ()
pressCombo tui modifiers key =
  appendAction tui $
    "pressCombo "
      <> T.pack (intercalate "+" (map show modifiers))
      <> "+"
      <> renderKey key

typeText :: Tui -> Text -> IO ()
typeText tui textValue =
  modifyState tui $ \state ->
    state
      { visibleBuffer = visibleBuffer state <> textValue
      , actionLog = actionLog state <> ["typeText " <> textValue]
      }

expectVisible :: Tui -> Selector -> IO ()
expectVisible tui selector =
  waitFor tui defaultWaitOptions (selectorMatches selector)

expectNotVisible :: Tui -> Selector -> IO ()
expectNotVisible tui selector =
  waitFor tui defaultWaitOptions (not . selectorMatches selector)

waitForText :: Tui -> Selector -> IO ()
waitForText tui selector =
  waitFor tui defaultWaitOptions (selectorMatches selector)

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
  appendAction tui ("expectSnapshot " <> unSnapshotName snapshotName)

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

runOneSpec :: RunOptions -> FilePath -> Spec -> IO TestResult
runOneSpec options runRoot specDef = do
  start <- getCurrentTime
  (status, finalState, attempts) <- executeWithRetries options specDef
  end <- getCurrentTime
  let slug = slugify (specName specDef)
  let testRoot = runRoot </> "tests" </> slug </> "attempt-final"
  createDirectoryIfMissing True testRoot
  emitTestArtifacts testRoot finalState status
  pure
    TestResult
      { resultName = specName specDef
      , resultSlug = slug
      , resultStatus = status
      , resultDurationMs = floor (diffUTCTime end start * 1000)
      , resultAttempts = attempts
      }

executeWithRetries :: RunOptions -> Spec -> IO (TestStatus, TuiState, Int)
executeWithRetries options specDef = go 1
 where
  maxAttempts = max 1 (retries options + 1)

  go attempt = do
    tui <- mkTui options (specName specDef)
    runResult <- try (specBody specDef tui)
    state <- readIORef (tuiStateRef tui)
    case runResult of
      Right () -> pure (Passed, state, attempt)
      Left (err :: SomeException)
        | attempt < maxAttempts -> go (attempt + 1)
        | otherwise -> pure (Failed (T.pack (displayException err)), state, attempt)

mkTui :: RunOptions -> String -> IO Tui
mkTui options name = do
  stateRef <-
    newIORef
      TuiState
        { launchedApp = Nothing
        , visibleBuffer = ""
        , actionLog = []
        }
  pure
    Tui
      { tuiName = name
      , tuiOptions = options
      , tuiStateRef = stateRef
      }

currentViewport :: Tui -> IO Viewport
currentViewport tui = do
  state <- readIORef (tuiStateRef tui)
  pure
    Viewport
      { viewportCols = terminalCols (tuiOptions tui)
      , viewportRows = terminalRows (tuiOptions tui)
      , viewportText = visibleBuffer state
      }

modifyState :: Tui -> (TuiState -> TuiState) -> IO ()
modifyState tui updateFn = modifyIORef' (tuiStateRef tui) updateFn

appendAction :: Tui -> Text -> IO ()
appendAction tui actionText =
  modifyState tui $ \state ->
    state {actionLog = actionLog state <> [actionText]}

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

matchCount :: Selector -> Viewport -> Int
matchCount selector viewport =
  case selector of
    Exact textValue -> occurrenceCount textValue (viewportText viewport)
    Regex patternText ->
      let alternatives = filter (not . T.null) (map cleanPattern (T.splitOn "|" patternText))
       in length [alt | alt <- alternatives, wildcardContains alt (viewportText viewport)]
    At _ _ ->
      if selectorMatches selector viewport then 1 else 0
    Within _ _ ->
      if selectorMatches selector viewport then 1 else 0
    Nth _ _ ->
      if selectorMatches selector viewport then 1 else 0

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
    let (before, after) = T.breakOn needle value
     in if T.null after
          then count
          else go (count + 1) (T.drop (T.length needle) (before <> after))

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

emitTestArtifacts :: FilePath -> TuiState -> TestStatus -> IO ()
emitTestArtifacts testRoot state status = do
  writeFile (testRoot </> "viewport.txt") (T.unpack (visibleBuffer state))
  writeFile (testRoot </> "session.cast") "# starter placeholder cast\n"
  writeFile (testRoot </> "steps.json") (renderStepsJson state status)
  unless (isNothing (launchedApp state)) $
    writeFile (testRoot </> "launch.txt") (show (launchedApp state))

renderStepsJson :: TuiState -> TestStatus -> String
renderStepsJson state status =
  "{\n"
    <> "  \"status\": "
    <> quoted (statusText status)
    <> ",\n"
    <> "  \"steps\": [\n"
    <> intercalate ",\n" (map renderStep (zip [1 :: Int ..] (actionLog state)))
    <> "\n  ]\n"
    <> "}\n"
 where
  renderStep (idx, actionText) =
    "    {\"id\": "
      <> show idx
      <> ", \"action\": "
      <> quoted (T.unpack actionText)
      <> "}"

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
      <> maybeErrorField (resultStatus result)
      <> "}"

  maybeErrorField Passed = ""
  maybeErrorField (Failed err) = ", \"error\": " <> quoted (T.unpack err)

  statusText Passed = "passed"
  statusText (Failed _) = "failed"

writeRunMarkdown :: FilePath -> [TestResult] -> IO ()
writeRunMarkdown runRoot results =
  writeFile (runRoot </> "report.md") $
    "# tuispec run report\n\n"
      <> "| Test | Status | Attempts | Duration (ms) |\n"
      <> "|---|---|---:|---:|\n"
      <> concatMap row results
 where
  row result =
    "| "
      <> resultName result
      <> " | "
      <> statusText (resultStatus result)
      <> " | "
      <> show (resultAttempts result)
      <> " | "
      <> show (resultDurationMs result)
      <> " |\n"

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

mkRunRoot :: RunOptions -> IO FilePath
mkRunRoot options = do
  createDirectoryIfMissing True (artifactsDir options)
  now <- getCurrentTime
  let stamp = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%SZ" now
  let runRoot = artifactsDir options </> ("run-" <> stamp)
  createDirectoryIfMissing True runRoot
  pure runRoot

slugify :: String -> String
slugify input =
  let lowered = map toLower input
      normalized = map normalize lowered
      compact = collapseDashes normalized
   in trimDashes compact
 where
  normalize c
    | isAlphaNum c = c
    | otherwise = '-'

  collapseDashes [] = []
  collapseDashes ('-' : '-' : rest) = collapseDashes ('-' : rest)
  collapseDashes (c : rest) = c : collapseDashes rest

  trimDashes = reverse . dropWhile (== '-') . reverse . dropWhile (== '-')

quoted :: String -> String
quoted value = "\"" <> concatMap escape value <> "\""
 where
  escape '"' = "\\\""
  escape '\\' = "\\\\"
  escape '\n' = "\\n"
  escape '\r' = "\\r"
  escape '\t' = "\\t"
  escape c = [c]
