{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : TuiSpec.Render
Description : Render snapshot ANSI artifacts as PNG or plain text.

Both render paths auto-detect viewport size from adjacent @.meta.json@
files produced by 'TuiSpec.Runner.expectSnapshot'. Explicit CLI overrides
can still be supplied when needed.
-}
module TuiSpec.Render (
    renderAnsiSnapshotFile,
    renderAnsiSnapshotFileWithFont,
    renderAnsiSnapshotTextFile,
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch, throwIO)
import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.List (intercalate, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory)
import System.IO (hClose, openTempFile)
import System.Process (proc, readCreateProcessWithExitCode)
import Text.Read (readMaybe)
import TuiSpec.Runner (renderAnsiViewportText, serializeAnsiSnapshot)
import TuiSpec.Types (defaultRunOptions, terminalCols, terminalRows)

{- | Render an ANSI snapshot file to PNG.

Size resolution order for rows/cols:

1. explicit overrides
2. adjacent @.meta.json@
3. 'defaultRunOptions'

Theme is rendering-only and defaults to @auto@ when omitted.
-}
renderAnsiSnapshotFile :: Maybe Int -> Maybe Int -> Maybe String -> FilePath -> FilePath -> IO ()
renderAnsiSnapshotFile rowOverride colOverride themeOverride ansiPath outPath = do
    renderAnsiSnapshotFileWithFont Nothing rowOverride colOverride themeOverride ansiPath outPath

renderAnsiSnapshotFileWithFont :: Maybe FilePath -> Maybe Int -> Maybe Int -> Maybe String -> FilePath -> FilePath -> IO ()
renderAnsiSnapshotFileWithFont maybeFontPath rowOverride colOverride themeOverride ansiPath outPath = do
    ansiText <- TIO.readFile ansiPath
    (rows, cols, effectiveTheme) <- resolveRenderOptions rowOverride colOverride themeOverride ansiPath
    let payload = serializeAnsiSnapshot rows cols effectiveTheme ansiText
    fontPath <- resolveFontPath maybeFontPath
    createDirectoryIfMissing True (takeDirectory outPath)
    (tmpInPath, tmpHandle) <- openTempFile (takeDirectory outPath) "snapshot-styled-"
    TIO.hPutStr tmpHandle (T.pack payload)
    hClose tmpHandle
    let args = ["-c", pythonStyledRenderScript, tmpInPath, outPath, fontPath]
    result <- readCreateProcessWithExitCode (proc "python3" args) ""
    ignoreIOError (removeFile tmpInPath)
    case result of
        (ExitSuccess, _, _) -> pure ()
        (_, _, stderrText) ->
            throwIO
                (userError ("Failed to render styled PNG (python3 + Pillow required in PATH). " <> stderrText))

{- | Render an ANSI snapshot file to visible plain text.

This replays ANSI into the same emulator used by snapshot comparison and
therefore preserves terminal layout semantics better than naive escape
stripping.
-}
renderAnsiSnapshotTextFile :: Maybe Int -> Maybe Int -> FilePath -> FilePath -> IO ()
renderAnsiSnapshotTextFile rowOverride colOverride ansiPath outPath = do
    ansiText <- TIO.readFile ansiPath
    (rows, cols, _theme) <- resolveRenderOptions rowOverride colOverride Nothing ansiPath
    createDirectoryIfMissing True (takeDirectory outPath)
    TIO.writeFile outPath (renderAnsiViewportText rows cols ansiText)

resolveRenderOptions :: Maybe Int -> Maybe Int -> Maybe String -> FilePath -> IO (Int, Int, String)
resolveRenderOptions rowOverride colOverride themeOverride ansiPath = do
    metadata <- loadSnapshotMetadata ansiPath
    colorFgBgValue <- lookupEnv "COLORFGBG"
    let fallbackRows = terminalRows defaultRunOptions
    let fallbackCols = terminalCols defaultRunOptions
    let metadataRowsValue = metadataRows <$> metadata
    let metadataColsValue = metadataCols <$> metadata
    let rows =
            fromMaybe fallbackRows $
                case rowOverride of
                    Just value -> Just value
                    Nothing -> metadataRowsValue
    let cols =
            fromMaybe fallbackCols $
                case colOverride of
                    Just value -> Just value
                    Nothing -> metadataColsValue
    let requestedTheme = fromMaybe "auto" themeOverride
    let effectiveTheme = resolveAutoSnapshotTheme requestedTheme colorFgBgValue
    pure (rows, cols, effectiveTheme)

data SnapshotMetadata = SnapshotMetadata
    { metadataRows :: Int
    , metadataCols :: Int
    }

instance FromJSON SnapshotMetadata where
    parseJSON =
        withObject "SnapshotMetadata" $ \objectValue ->
            SnapshotMetadata
                <$> objectValue .: "rows"
                <*> objectValue .: "cols"

loadSnapshotMetadata :: FilePath -> IO (Maybe SnapshotMetadata)
loadSnapshotMetadata ansiPath = do
    let metadataPath = snapshotMetadataPath ansiPath
    exists <- doesFileExist metadataPath
    if not exists
        then pure Nothing
        else do
            metadataBytes <- BL.readFile metadataPath
            pure $
                case eitherDecode metadataBytes of
                    Left _ -> Nothing
                    Right metadataValue
                        | metadataRows metadataValue > 0
                            && metadataCols metadataValue > 0 ->
                            Just metadataValue
                    Right _ -> Nothing

snapshotMetadataPath :: FilePath -> FilePath
snapshotMetadataPath ansiPath =
    if ".ansi.txt" `isSuffixOf` ansiPath
        then take (length ansiPath - length (".ansi.txt" :: String)) ansiPath <> ".meta.json"
        else ansiPath <> ".meta.json"

pythonStyledRenderScript :: String
pythonStyledRenderScript =
    unlines
        [ "import json"
        , "import sys"
        , "from PIL import Image, ImageDraw, ImageFont"
        , "inp, out = sys.argv[1], sys.argv[2]"
        , "font_path = sys.argv[3]"
        , "font = ImageFont.truetype(font_path, 12)"
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

resolveFontPath :: Maybe FilePath -> IO FilePath
resolveFontPath maybeFontPath = do
    envFont <- lookupEnv "TUISPEC_FONT_PATH"
    case maybeFontPath <|> envFont of
        Just pathValue -> do
            exists <- doesFileExist pathValue
            if exists
                then pure pathValue
                else
                    throwIO
                        ( userError
                            ( "Font not found at "
                                <> pathValue
                                <> ". Pass --font PATH or set TUISPEC_FONT_PATH to a valid file."
                            )
                        )
        Nothing -> do
            maybeFont <- findFirstExistingPath defaultFallbackFontPaths
            case maybeFont of
                Just fontPath -> pure fontPath
                Nothing ->
                    throwIO
                        ( userError
                            ( "Font not found. Looked for: "
                                <> intercalate ", " defaultFallbackFontPaths
                                <> ". Pass --font PATH or set TUISPEC_FONT_PATH to override."
                            )
                        )

findFirstExistingPath :: [FilePath] -> IO (Maybe FilePath)
findFirstExistingPath paths =
    case paths of
        [] -> pure Nothing
        candidate : rest -> do
            exists <- doesFileExist candidate
            if exists
                then pure (Just candidate)
                else findFirstExistingPath rest

defaultFallbackFontPaths :: [FilePath]
defaultFallbackFontPaths =
    [ "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    , "/usr/share/fonts/dejavu/DejaVuSansMono.ttf"
    , "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
    , "/usr/share/fonts/truetype/liberation2/LiberationMono-Regular.ttf"
    , "/usr/share/fonts/liberation/LiberationMono-Regular.ttf"
    , "/System/Library/Fonts/Menlo.ttc"
    , "/Library/Fonts/Menlo.ttc"
    ]

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
