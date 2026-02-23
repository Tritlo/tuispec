{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TuiSpec.ProjectRoot
Description : Shared helpers for resolving the active project root.
-}
module TuiSpec.ProjectRoot (
    resolveProjectRoot,
) where

import Control.Exception (SomeException, catch)
import Data.List (isSuffixOf)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))

resolveProjectRoot :: IO FilePath
resolveProjectRoot = do
    projectRootOverride <- lookupEnv "TUISPEC_PROJECT_ROOT"
    case projectRootOverride of
        Just override -> canonicalizePath override
        Nothing -> do
            cwd <- getCurrentDirectory
            locateProjectRoot cwd

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
