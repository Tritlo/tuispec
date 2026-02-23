module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import Options.Applicative
import Paths_tuispec (version)
import System.IO (BufferMode (NoBuffering), hFlush, hIsTerminalDevice, hReady, hSetBuffering, hSetEcho, stdin, stdout)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (TerminalAttributes, getTerminalAttributes, setTerminalAttributes, withMinInput, withTime, withoutMode)
import System.Posix.Terminal qualified as Posix
import Text.Read (readMaybe)
import TuiSpec.Render (renderAnsiSnapshotFileWithFont, renderAnsiSnapshotTextFile)
import TuiSpec.Replay (ReplayFrame (..), ReplaySpeed (ReplayAsFastAsPossible, ReplayRealTime), loadReplayFrames, streamReplayFrames, streamReplayRequests)
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
    , replayShowInput :: Bool
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
            isTTY <- hIsTerminalDevice stdin
            if isTTY
                then replayInteractive options
                else replayNonInteractive options

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
                <> value ReplayRealTime
                <> showDefaultWith renderReplaySpeed
            )
        <*> switch
            ( long "show-input"
                <> help "Show last input action on a status line below the viewport"
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

-- --------------------------------------------------------------------
-- Replay modes
-- --------------------------------------------------------------------

-- | Interactive replay with keyboard controls (when stdin is a TTY).
replayInteractive :: ReplayOptions -> IO ()
replayInteractive options = do
    frames <- loadReplayFrames (replayInputPath options)
    if null frames
        then do
            replayed <- streamReplayRequests (replaySpeed options) (replayInputPath options) TIO.putStrLn
            putStrLn ("Replayed " <> show replayed <> " request messages")
        else withAltScreen $
            withRawTerminal $ do
                interactiveReplay (replaySpeed options) (replayShowInput options) frames
                putStrLn ("Replayed " <> show (length frames) <> " frames")

-- | Non-interactive streaming replay (when stdin is not a TTY).
replayNonInteractive :: ReplayOptions -> IO ()
replayNonInteractive options = do
    let callback = if replayShowInput options then displayFrameWithInput else displayFrameOnly
    frameCount <- streamReplayFrames (replaySpeed options) (replayInputPath options) callback
    if frameCount > 0
        then putStrLn ("Replayed " <> show frameCount <> " frames")
        else do
            replayed <- streamReplayRequests (replaySpeed options) (replayInputPath options) TIO.putStrLn
            putStrLn ("Replayed " <> show replayed <> " request messages")

-- | Display a frame ignoring input labels (non-interactive mode).
displayFrameOnly :: Text -> Maybe Text -> IO ()
displayFrameOnly frameText _maybeInput = do
    putStr "\ESC[?25l\ESC[H"
    TIO.putStr frameText
    putStr "\ESC[?25h"
    hFlush stdout

-- | Display a frame with an input status line (non-interactive mode).
displayFrameWithInput :: Text -> Maybe Text -> IO ()
displayFrameWithInput frameText maybeInput = do
    putStr "\ESC[?25l\ESC[H"
    TIO.putStr frameText
    putStr "\ESC[K"
    case maybeInput of
        Just label -> do
            putStr "\n\ESC[7m "
            TIO.putStr label
            putStr " \ESC[0m\ESC[K"
        Nothing -> putStr "\n\ESC[K"
    putStr "\ESC[?25h"
    hFlush stdout

-- --------------------------------------------------------------------
-- Interactive replay
-- --------------------------------------------------------------------

-- | Keypress actions recognised by the interactive replay loop.
data ReplayKey
    = KeySpace
    | KeyLeft
    | KeyRight
    | KeyUp
    | KeyDown
    | KeyQuit
    | KeyUnknown

-- | Enter the alternate screen buffer and leave it when the action finishes.
withAltScreen :: IO a -> IO a
withAltScreen =
    bracket_
        (putStr "\ESC[?1049h\ESC[2J\ESC[H" >> hFlush stdout)
        (putStr "\ESC[?25h\ESC[?1049l" >> hFlush stdout)

-- | Set the terminal to raw mode and restore it when the action finishes.
withRawTerminal :: IO a -> IO a
withRawTerminal act = do
    oldAttrs <- getTerminalAttributes stdInput
    let rawAttrs =
            withTime (withMinInput (setRaw oldAttrs) 0) 0
    bracket_
        (setTerminalAttributes stdInput rawAttrs Posix.Immediately >> hSetBuffering stdin NoBuffering >> hSetEcho stdin False)
        (setTerminalAttributes stdInput oldAttrs Posix.Immediately)
        act

-- | Strip terminal attributes down to raw mode (no echo, no canonical, no signals).
setRaw :: TerminalAttributes -> TerminalAttributes
setRaw attrs =
    foldl
        withoutMode
        attrs
        [ Posix.EnableEcho
        , Posix.ProcessInput
        , Posix.KeyboardInterrupts
        , Posix.StartStopOutput
        , Posix.ExtendedFunctions
        ]

-- | Run the interactive replay loop over a non-empty list of frames.
interactiveReplay :: ReplaySpeed -> Bool -> [ReplayFrame] -> IO ()
interactiveReplay speed showInput frameList = do
    let frames = listToIndexed frameList
        totalFrames = length frameList
    case frameList of
        [] -> pure () -- unreachable, caller checks null
        (first : _) -> do
            displayFrame showInput first 0 totalFrames False
            go frames 0 totalFrames False
  where
    go frames idx total paused = do
        key <- if paused then readKey else waitForKeyOrDelay speed frames idx
        case key of
            Just KeyQuit -> pure ()
            Just KeySpace -> do
                let newPaused = not paused
                displayFrame showInput (snd (frames !! idx)) idx total newPaused
                go frames idx total newPaused
            Just KeyLeft -> do
                let newIdx = max 0 (idx - 1)
                displayFrame showInput (snd (frames !! newIdx)) newIdx total paused
                go frames newIdx total paused
            Just KeyRight -> do
                let newIdx = min (total - 1) (idx + 1)
                displayFrame showInput (snd (frames !! newIdx)) newIdx total paused
                go frames newIdx total paused
            Just KeyUp -> do
                let newIdx = max 0 (idx - 5)
                displayFrame showInput (snd (frames !! newIdx)) newIdx total paused
                go frames newIdx total paused
            Just KeyDown -> do
                let newIdx = min (total - 1) (idx + 5)
                displayFrame showInput (snd (frames !! newIdx)) newIdx total paused
                go frames newIdx total paused
            Just KeyUnknown -> go frames idx total paused
            Nothing ->
                -- Timeout expired, advance to next frame
                if idx + 1 >= total
                    then do
                        -- Reached the end, pause on last frame
                        displayFrame showInput (snd (frames !! idx)) idx total True
                        go frames idx total True
                    else do
                        let newIdx = idx + 1
                        displayFrame showInput (snd (frames !! newIdx)) newIdx total False
                        go frames newIdx total False

-- | Display a single frame with optional input label and status bar.
displayFrame :: Bool -> ReplayFrame -> Int -> Int -> Bool -> IO ()
displayFrame showInput frame idx total paused = do
    putStr "\ESC[?25l\ESC[H"
    TIO.putStr (replayFrameText frame)
    putStr "\ESC[K"
    if showInput
        then do
            putStr "\n"
            case replayFrameInput frame of
                Just label -> do
                    putStr "\ESC[7m "
                    TIO.putStr label
                    putStr " \ESC[0m"
                Nothing -> pure ()
            putStr "\ESC[K"
        else putStr "\n\ESC[K"
    -- Status bar: frame counter and pause state
    putStr "\n\ESC[7m "
    putStr (show (idx + 1) <> "/" <> show total)
    if paused then putStr " [PAUSED] Space:play Left/Right:step Up/Down:skip5 q:quit" else pure ()
    putStr " \ESC[0m\ESC[K"
    hFlush stdout

-- | Blocking read of a single keypress. Returns 'Just' the key.
readKey :: IO (Maybe ReplayKey)
readKey = Just <$> readKeyRaw

-- | Read a raw keypress, handling escape sequences for arrow keys.
readKeyRaw :: IO ReplayKey
readKeyRaw = do
    c <- getChar
    case c of
        'q' -> pure KeyQuit
        ' ' -> pure KeySpace
        '\ESC' -> do
            -- Could be an arrow key escape sequence
            ready <- hReady stdin
            if ready
                then do
                    c2 <- getChar
                    if c2 == '['
                        then do
                            c3 <- getChar
                            case c3 of
                                'A' -> pure KeyUp
                                'B' -> pure KeyDown
                                'C' -> pure KeyRight
                                'D' -> pure KeyLeft
                                _ -> pure KeyUnknown
                        else pure KeyUnknown
                else pure KeyQuit -- bare Escape = quit
        _ -> pure KeyUnknown

{- | Wait for a keypress or the inter-frame delay, whichever comes first.
Returns 'Nothing' if the delay expired (advance frame), 'Just' if a key was pressed.
-}
waitForKeyOrDelay :: ReplaySpeed -> [(Int, ReplayFrame)] -> Int -> IO (Maybe ReplayKey)
waitForKeyOrDelay speed frames idx = do
    let delayMicros = case speed of
            ReplayAsFastAsPossible -> 0 :: Int
            ReplayRealTime ->
                if idx + 1 < length frames
                    then
                        let current = snd (frames !! idx)
                            next = snd (frames !! (idx + 1))
                         in fromIntegral (replayFrameTimestampMicros next - replayFrameTimestampMicros current)
                    else 0
    if delayMicros <= 0
        then do
            ready <- hReady stdin
            if ready then Just <$> readKeyRaw else pure Nothing
        else waitWithTimeout delayMicros

{- | Wait up to the given number of microseconds for input. Returns 'Nothing'
on timeout (meaning we should advance), or 'Just key' if a key was pressed.
-}
waitWithTimeout :: Int -> IO (Maybe ReplayKey)
waitWithTimeout totalMicros = go totalMicros
  where
    chunkMs = 20 -- poll every 20ms
    chunkMicros = chunkMs * 1000
    go remaining
        | remaining <= 0 = do
            ready <- hReady stdin
            if ready then Just <$> readKeyRaw else pure Nothing
        | otherwise = do
            ready <- hReady stdin
            if ready
                then Just <$> readKeyRaw
                else do
                    let wait = min remaining chunkMicros
                    threadDelay wait
                    go (remaining - wait)

-- | Index a list with positions.
listToIndexed :: [a] -> [(Int, a)]
listToIndexed = zip [0 ..]
