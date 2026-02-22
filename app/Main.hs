module Main where

import Control.Exception (SomeException, displayException, try)
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure, exitWith)
import System.Process (callProcess)

data Command
  = Run FilePath [String]
  | ListTests FilePath
  | Help
  deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  case parseCommand args of
    Left err -> do
      putStrLn err
      putStrLn usage
      exitFailure
    Right Help -> putStrLn usage
    Right (ListTests path) -> do
      listResult <- try (readFile path) :: IO (Either SomeException String)
      case listResult of
        Left err -> do
          putStrLn ("Failed to read " <> path <> ": " <> displayException err)
          exitWith (ExitFailure 2)
        Right content -> do
          let names = extractTestNames (lines content)
          mapM_ putStrLn names
    Right (Run path passthrough) -> do
      runResult <-
        try (callProcess "cabal" (["exec", "runghc", "--", "-isrc", path] <> passthrough))
          :: IO (Either SomeException ())
      case runResult of
        Left err -> do
          putStrLn ("tuitest run failed (via cabal exec runghc): " <> displayException err)
          exitWith (ExitFailure 2)
        Right () -> pure ()

parseCommand :: [String] -> Either String Command
parseCommand args =
  case args of
    [] -> Right Help
    ["--help"] -> Right Help
    ["help"] -> Right Help
    "list" : path : _ -> Right (ListTests path)
    "run" : path : rest ->
      let passthrough = stripKnownFlags rest
       in Right (Run path passthrough)
    _ -> Left "Unknown command."

stripKnownFlags :: [String] -> [String]
stripKnownFlags [] = []
stripKnownFlags ("--timeout" : _value : rest) = stripKnownFlags rest
stripKnownFlags ("--retries" : _value : rest) = stripKnownFlags rest
stripKnownFlags ("--step-retries" : _value : rest) = stripKnownFlags rest
stripKnownFlags ("--snapshot-source" : _value : rest) = stripKnownFlags rest
stripKnownFlags ("--artifacts-dir" : _value : rest) = stripKnownFlags rest
stripKnownFlags ("--json" : _value : rest) = stripKnownFlags rest
stripKnownFlags ("--markdown-report" : _value : rest) = stripKnownFlags rest
stripKnownFlags ("--update-snapshots" : rest) = stripKnownFlags rest
stripKnownFlags (value : rest)
  | "--" `isPrefixOf` value = stripKnownFlags rest
  | otherwise = value : stripKnownFlags rest

extractTestNames :: [String] -> [String]
extractTestNames = foldr extract []
 where
  extract line acc =
    case breakOnTest line of
      Nothing -> acc
      Just testName -> testName : acc

  breakOnTest line =
    let marker = "test \""
     in case dropUntil marker line of
          Nothing -> Nothing
          Just remaining -> Just (takeWhile (/= '"') remaining)

dropUntil :: String -> String -> Maybe String
dropUntil needle haystack =
  case haystack of
    [] -> Nothing
    _ | needle `isPrefixOf` haystack -> Just (drop (length needle) haystack)
    _ : rest -> dropUntil needle rest

usage :: String
usage =
  unlines
    [ "tuitest - starter CLI"
    , ""
    , "Usage:"
    , "  tuitest run <Spec.hs> [flags]"
    , "  tuitest list <Spec.hs>"
    , ""
    , "Supported flag shape (currently consumed by future runner wiring):"
    , "  --timeout <seconds>"
    , "  --retries <n>"
    , "  --step-retries <n>"
    , "  --snapshot-source tmux|asciinema"
    , "  --update-snapshots"
    , "  --artifacts-dir <path>"
    , "  --json <path>"
    , "  --markdown-report <path>"
    ]
