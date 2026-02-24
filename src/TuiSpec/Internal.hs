{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TuiSpec.Internal
Description : Shared internal utilities used across tuispec modules.

This module is not part of the public API. It collects helper functions
that are needed by more than one of 'TuiSpec.Runner', 'TuiSpec.Server',
and 'TuiSpec.Render'.
-}
module TuiSpec.Internal (
    -- * List helpers
    safeIndex,

    -- * Path helpers
    safeFileStem,
    snapshotMetadataPath,

    -- * Pattern matching
    regexLikeMatch,
    cleanPattern,
    wildcardContains,

    -- * Theme helpers
    resolveAutoSnapshotTheme,
    detectTerminalBackground,

    -- * Exception helpers
    ignoreIOError,
) where

import Control.Exception (SomeException, catch)
import Data.Char (isAlphaNum, toLower)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | Safe list indexing that returns 'Nothing' for out-of-bounds access.
safeIndex :: Int -> [a] -> Maybe a
safeIndex indexValue values
    | indexValue < 0 = Nothing
    | otherwise = go indexValue values
  where
    go _ [] = Nothing
    go 0 (x : _) = Just x
    go n (_ : xs) = go (n - 1) xs

-- | Slugify a string into a safe file stem (lowercase, alphanumeric + dashes).
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

-- | Derive the @.meta.json@ sidecar path from an @.ansi.txt@ snapshot path.
snapshotMetadataPath :: FilePath -> FilePath
snapshotMetadataPath ansiPath =
    if ".ansi.txt" `isSuffixOf` ansiPath
        then take (length ansiPath - length (".ansi.txt" :: String)) ansiPath <> ".meta.json"
        else ansiPath <> ".meta.json"

{- | Check whether a simple regex-like pattern matches a haystack.

Supports @|@ alternation and @.*@ wildcards.
-}
regexLikeMatch :: Text -> Text -> Bool
regexLikeMatch patternText haystack =
    any (`wildcardContains` haystack) alternatives
  where
    alternatives =
        filter (not . T.null) $
            map cleanPattern (T.splitOn "|" patternText)

-- | Strip parentheses from a pattern fragment.
cleanPattern :: Text -> Text
cleanPattern = T.filter (`notElem` ("()" :: String))

-- | Check whether a @.*@-delimited pattern matches within a haystack.
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

-- | Resolve @\"auto\"@ snapshot theme to a concrete theme name using @COLORFGBG@.
resolveAutoSnapshotTheme :: String -> Maybe String -> String
resolveAutoSnapshotTheme requestedTheme colorFgBgValue =
    case map toLower requestedTheme of
        "auto" ->
            case detectTerminalBackground colorFgBgValue of
                Just "light" -> "pty-default-light"
                _ -> "pty-default-dark"
        _ -> requestedTheme

-- | Detect terminal background lightness from the @COLORFGBG@ value.
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

-- | Run an IO action, silently discarding any exceptions.
ignoreIOError :: IO () -> IO ()
ignoreIOError action =
    action `catch` \(_ :: SomeException) -> pure ()
