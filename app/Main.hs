module Main where

import Data.List (isSuffixOf)
import Options.Applicative
import Text.Read (readMaybe)
import TuiSpec.Render (renderAnsiSnapshotFile, renderAnsiSnapshotTextFile)

data Command
    = Render RenderOptions
    | RenderText RenderTextOptions

data RenderOptions = RenderOptions
    { inputPath :: FilePath
    , outputPath :: Maybe FilePath
    , themeName :: Maybe String
    , cols :: Maybe Int
    , rows :: Maybe Int
    }

data RenderTextOptions = RenderTextOptions
    { textInputPath :: FilePath
    , textOutputPath :: Maybe FilePath
    , textCols :: Maybe Int
    , textRows :: Maybe Int
    }

main :: IO ()
main = do
    parsedCommand <- execParser commandParserInfo
    runCommand parsedCommand

runCommand :: Command -> IO ()
runCommand parsedCommand =
    case parsedCommand of
        Render options -> do
            let outPath = maybe (defaultOutputPath (inputPath options)) id (outputPath options)
            renderAnsiSnapshotFile (rows options) (cols options) (themeName options) (inputPath options) outPath
            putStrLn ("Rendered " <> outPath)
        RenderText options -> do
            let outPath = maybe (defaultTextOutputPath (textInputPath options)) id (textOutputPath options)
            renderAnsiSnapshotTextFile (textRows options) (textCols options) (textInputPath options) outPath
            putStrLn ("Rendered " <> outPath)

commandParserInfo :: ParserInfo Command
commandParserInfo =
    info
        (parseCommand <**> helper)
        ( fullDesc
            <> progDesc "Render ANSI snapshot artifacts to PNG"
            <> header "tuispec"
        )

parseCommand :: Parser Command
parseCommand =
    hsubparser
        ( command
            "render"
            ( info
                (Render <$> parseRenderOptions)
                (progDesc "Render a .ansi.txt snapshot file")
            )
            <> command
                "render-text"
                ( info
                    (RenderText <$> parseRenderTextOptions)
                    (progDesc "Render visible plain text from a .ansi.txt snapshot file using terminal emulation")
                )
        )

parseRenderOptions :: Parser RenderOptions
parseRenderOptions =
    RenderOptions
        <$> argument
            str
            ( metavar "SNAPSHOT_ANSI_TXT"
                <> help "Path to snapshot ANSI file"
            )
        <*> optional
            ( strOption
                ( long "out"
                    <> metavar "OUTPUT_PNG"
                    <> help "Output PNG path"
                )
            )
        <*> optional
            ( strOption
                ( long "theme"
                    <> metavar "THEME"
                    <> help "Theme override: auto|dark|light (default: metadata, then auto)"
                )
            )
        <*> optional
            ( option
                positiveIntReader
                ( long "cols"
                    <> metavar "N"
                    <> help "Viewport columns override (default: metadata, then 134)"
                )
            )
        <*> optional
            ( option
                positiveIntReader
                ( long "rows"
                    <> metavar "N"
                    <> help "Viewport rows override (default: metadata, then 40)"
                )
            )

parseRenderTextOptions :: Parser RenderTextOptions
parseRenderTextOptions =
    RenderTextOptions
        <$> argument
            str
            ( metavar "SNAPSHOT_ANSI_TXT"
                <> help "Path to snapshot ANSI file"
            )
        <*> optional
            ( strOption
                ( long "out"
                    <> metavar "OUTPUT_TXT"
                    <> help "Output plain text path"
                )
            )
        <*> optional
            ( option
                positiveIntReader
                ( long "cols"
                    <> metavar "N"
                    <> help "Viewport columns override (default: metadata, then 134)"
                )
            )
        <*> optional
            ( option
                positiveIntReader
                ( long "rows"
                    <> metavar "N"
                    <> help "Viewport rows override (default: metadata, then 40)"
                )
            )

positiveIntReader :: ReadM Int
positiveIntReader =
    eitherReader $ \raw ->
        case readMaybe raw :: Maybe Int of
            Just parsed | parsed > 0 -> Right parsed
            _ -> Left "Expected a positive integer"

defaultOutputPath :: FilePath -> FilePath
defaultOutputPath input =
    if ".ansi.txt" `isSuffixOf` input
        then take (length input - length (".ansi.txt" :: String)) input <> ".png"
        else input <> ".png"

defaultTextOutputPath :: FilePath -> FilePath
defaultTextOutputPath input =
    if ".ansi.txt" `isSuffixOf` input
        then take (length input - length (".ansi.txt" :: String)) input <> ".txt"
        else input <> ".txt"
