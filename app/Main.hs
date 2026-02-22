module Main where

import Control.Exception (SomeException, displayException, try)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (
    canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
 )
import System.Environment (getArgs, setEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeDirectory, (</>))
import System.Process (callProcess)

data Command
    = Run FilePath [String] [(String, String)]
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
        Right (Run path passthrough envOverrides) -> do
            mapM_ (uncurry setEnv) envOverrides
            rootResult <- try (setProjectRootFromSpec path) :: IO (Either SomeException ())
            case rootResult of
                Left _ -> pure ()
                Right () -> pure ()
            runResult <-
                try (callProcess "cabal" (["exec", "runghc", "--", "-isrc", path] <> passthrough)) ::
                    IO (Either SomeException ())
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
            let (envOverrides, passthrough) = parseRunFlags rest
             in Right (Run path passthrough envOverrides)
        _ -> Left "Unknown command."

parseRunFlags :: [String] -> ([(String, String)], [String])
parseRunFlags = go [] []
  where
    go envAcc passAcc args =
        case args of
            [] -> (reverse envAcc, reverse passAcc)
            "--timeout" : value : rest ->
                go (("TUISPEC_TIMEOUT_SECONDS", value) : envAcc) passAcc rest
            "--retries" : value : rest ->
                go (("TUISPEC_RETRIES", value) : envAcc) passAcc rest
            "--step-retries" : value : rest ->
                go (("TUISPEC_STEP_RETRIES", value) : envAcc) passAcc rest
            "--artifacts-dir" : value : rest ->
                go (("TUISPEC_ARTIFACTS_DIR", value) : envAcc) passAcc rest
            "--update-snapshots" : rest ->
                go (("TUISPEC_UPDATE_SNAPSHOTS", "true") : envAcc) passAcc rest
            "--snapshot-theme" : value : rest ->
                go (("TUISPEC_SNAPSHOT_THEME", value) : envAcc) passAcc rest
            "--first-match" : rest ->
                go (("TUISPEC_AMBIGUITY_MODE", "first") : envAcc) passAcc rest
            "--fail-on-ambiguous" : rest ->
                go (("TUISPEC_AMBIGUITY_MODE", "fail") : envAcc) passAcc rest
            value : rest
                | "--" `isPrefixOf` value ->
                    go envAcc (value : passAcc) rest
                | otherwise ->
                    go envAcc (value : passAcc) rest

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

setProjectRootFromSpec :: FilePath -> IO ()
setProjectRootFromSpec specPath = do
    specAbsolutePath <- canonicalizePath specPath
    root <- findProjectRoot (takeDirectory specAbsolutePath)
    setEnv "TUISPEC_PROJECT_ROOT" root

findProjectRoot :: FilePath -> IO FilePath
findProjectRoot startDir = do
    absoluteStart <- canonicalizePath startDir
    go absoluteStart absoluteStart
  where
    go startRoot dir = do
        rootFound <- isProjectRoot dir
        if rootFound
            then pure dir
            else do
                let parent = takeDirectory dir
                if parent == dir
                    then pure startRoot
                    else go startRoot parent

isProjectRoot :: FilePath -> IO Bool
isProjectRoot dir = do
    hasGit <- doesDirectoryExist (dir </> ".git")
    hasCabalProject <- doesFileExist (dir </> "cabal.project")
    entries <- listDirectory dir
    let hasCabalFile = any (".cabal" `isSuffixOf`) entries
    pure (hasGit || hasCabalProject || hasCabalFile)

usage :: String
usage =
    unlines
        [ "tuitest - starter CLI"
        , ""
        , "Usage:"
        , "  tuitest run <Spec.hs> [flags]"
        , "  tuitest list <Spec.hs>"
        , ""
        , "Supported flags:"
        , "  --timeout <seconds>"
        , "  --retries <n>"
        , "  --step-retries <n>"
        , "  --update-snapshots"
        , "  --snapshot-theme <auto|dark|light>"
        , "  --artifacts-dir <path>"
        , "  --first-match"
        , "  --fail-on-ambiguous"
        ]
