module Main where

import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import Options.Applicative
import Paths_tuispec (version)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import TuiSpec.Render (renderAnsiSnapshotFileWithFont, renderAnsiSnapshotTextFile)
import TuiSpec.Replay (ReplaySpeed (ReplayAsFastAsPossible, ReplayRealTime), streamReplayFrames, streamReplayRequests)
import TuiSpec.Server qualified as Server
import TuiSpec.Types (AmbiguityMode (FailOnAmbiguous, FirstVisibleMatch, LastVisibleMatch))

data Command
    = Render RenderOptions
    | RenderText RenderTextOptions
    | Server ServerOptions
    | Replay ReplayOptions

data RenderOptions = RenderOptions
    { inputPath :: FilePath
    , outputPath :: Maybe FilePath
    , themeName :: Maybe String
    , fontPath :: Maybe FilePath
    , cols :: Maybe Int
    , rows :: Maybe Int
    }

data RenderTextOptions = RenderTextOptions
    { textInputPath :: FilePath
    , textOutputPath :: Maybe FilePath
    , textCols :: Maybe Int
    , textRows :: Maybe Int
    }

data ServerOptions = ServerOptions
    { artifactDir :: FilePath
    , serverCols :: Int
    , serverRows :: Int
    , timeoutSeconds :: Int
    , ambiguity :: AmbiguityMode
    }

data ReplayOptions = ReplayOptions
    { replayInputPath :: FilePath
    , replaySpeed :: ReplaySpeed
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
            renderAnsiSnapshotFileWithFont (fontPath options) (rows options) (cols options) (themeName options) (inputPath options) outPath
            putStrLn ("Rendered " <> outPath)
        RenderText options -> do
            let outPath = maybe (defaultTextOutputPath (textInputPath options)) id (textOutputPath options)
            renderAnsiSnapshotTextFile (textRows options) (textCols options) (textInputPath options) outPath
            putStrLn ("Rendered " <> outPath)
        Server options ->
            Server.runServer
                Server.ServerOptions
                    { Server.serverArtifactsDir = artifactDir options
                    , Server.serverTerminalCols = serverCols options
                    , Server.serverTerminalRows = serverRows options
                    , Server.serverTimeoutSeconds = timeoutSeconds options
                    , Server.serverAmbiguityMode = ambiguity options
                    }
        Replay options -> do
            frameCount <- streamReplayFrames (replaySpeed options) (replayInputPath options) displayFrame
            if frameCount > 0
                then putStrLn ("\nReplayed " <> show frameCount <> " frames")
                else do
                    replayed <- streamReplayRequests (replaySpeed options) (replayInputPath options) TIO.putStrLn
                    putStrLn ("Replayed " <> show replayed <> " request messages")

commandParserInfo :: ParserInfo Command
commandParserInfo =
    info
        (parseCommand <**> simpleVersioner (showVersion version) <**> helper)
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
            <> command
                "server"
                ( info
                    (Server <$> parseServerOptions)
                    (progDesc "Run JSON-RPC server on stdin/stdout for interactive TUI orchestration")
                )
            <> command
                "replay"
                ( info
                    (Replay <$> parseReplayOptions)
                    (progDesc "Replay request lines from a tuispec JSONL recording")
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
            ( strOption
                ( long "font"
                    <> metavar "FONT_PATH"
                    <> help "TTF/TTC font path override for PNG rendering (default: system fallback fonts)"
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

parseServerOptions :: Parser ServerOptions
parseServerOptions =
    ServerOptions
        <$> strOption
            ( long "artifact-dir"
                <> metavar "PATH"
                <> help "Artifacts directory base path for server sessions"
            )
        <*> option
            positiveIntReader
            ( long "cols"
                <> metavar "N"
                <> help "Default terminal columns"
                <> value 134
                <> showDefault
            )
        <*> option
            positiveIntReader
            ( long "rows"
                <> metavar "N"
                <> help "Default terminal rows"
                <> value 40
                <> showDefault
            )
        <*> option
            positiveIntReader
            ( long "timeout-seconds"
                <> metavar "N"
                <> help "Default timeout seconds"
                <> value 5
                <> showDefault
            )
        <*> option
            ambiguityModeReader
            ( long "ambiguity-mode"
                <> metavar "MODE"
                <> help "Default ambiguity mode: fail|first-visible|last-visible"
                <> value FailOnAmbiguous
                <> showDefaultWith renderAmbiguityMode
            )

parseReplayOptions :: Parser ReplayOptions
parseReplayOptions =
    ReplayOptions
        <$> argument
            str
            ( metavar "RECORDING_JSONL"
                <> help "Path to a recording JSONL file"
            )
        <*> option
            replaySpeedReader
            ( long "speed"
                <> metavar "MODE"
                <> help "Replay speed: as-fast-as-possible|real-time"
                <> value ReplayAsFastAsPossible
                <> showDefaultWith renderReplaySpeed
            )

positiveIntReader :: ReadM Int
positiveIntReader =
    eitherReader $ \raw ->
        case readMaybe raw :: Maybe Int of
            Just parsed | parsed > 0 -> Right parsed
            _ -> Left "Expected a positive integer"

ambiguityModeReader :: ReadM AmbiguityMode
ambiguityModeReader =
    eitherReader $ \raw ->
        case raw of
            "fail" -> Right FailOnAmbiguous
            "first-visible" -> Right FirstVisibleMatch
            "first" -> Right FirstVisibleMatch
            "last-visible" -> Right LastVisibleMatch
            "last" -> Right LastVisibleMatch
            _ -> Left "Expected ambiguity mode: fail|first-visible|last-visible"

renderAmbiguityMode :: AmbiguityMode -> String
renderAmbiguityMode mode =
    case mode of
        FailOnAmbiguous -> "fail"
        FirstVisibleMatch -> "first-visible"
        LastVisibleMatch -> "last-visible"

replaySpeedReader :: ReadM ReplaySpeed
replaySpeedReader =
    eitherReader $ \raw ->
        case raw of
            "as-fast-as-possible" -> Right ReplayAsFastAsPossible
            "real-time" -> Right ReplayRealTime
            _ -> Left "Expected replay speed: as-fast-as-possible|real-time"

renderReplaySpeed :: ReplaySpeed -> String
renderReplaySpeed speed =
    case speed of
        ReplayAsFastAsPossible -> "as-fast-as-possible"
        ReplayRealTime -> "real-time"

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

displayFrame :: Text -> IO ()
displayFrame frameText = do
    putStr "\ESC[2J\ESC[H"
    TIO.putStr frameText
    hFlush stdout
