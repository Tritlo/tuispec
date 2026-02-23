{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TuiSpec.Replay
Description : JSONL recording and replay primitives for server sessions.

This module isolates the JSONL event log format used by @tuispec server@
recording and replay features.
-}
module TuiSpec.Replay (
    RecordingHandle,
    RecordingEvent (..),
    RecordingDirection (..),
    ReplaySpeed (..),
    appendRecordingEvent,
    closeRecording,
    openRecording,
    streamReplayFrames,
    streamReplayRequests,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), eitherDecodeStrict', encode, object, withObject, (.:), (.=))
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (Handle, IOMode (AppendMode, ReadMode), hClose, hFlush, hIsEOF, hPutStrLn, openFile)

-- | Direction of a recorded JSON-RPC line, or a viewport frame capture.
data RecordingDirection
    = DirectionRequest
    | DirectionResponse
    | DirectionNotification
    | DirectionFrame
    deriving (Eq, Show)

instance ToJSON RecordingDirection where
    toJSON value =
        case value of
            DirectionRequest -> toJSON ("request" :: Text)
            DirectionResponse -> toJSON ("response" :: Text)
            DirectionNotification -> toJSON ("notification" :: Text)
            DirectionFrame -> toJSON ("frame" :: Text)

instance FromJSON RecordingDirection where
    parseJSON value = do
        raw <- parseJSON value
        case (T.toLower (T.strip raw) :: Text) of
            "request" -> pure DirectionRequest
            "response" -> pure DirectionResponse
            "notification" -> pure DirectionNotification
            "frame" -> pure DirectionFrame
            _ -> fail "direction must be one of: request, response, notification, frame"

-- | Single JSONL event written to disk.
data RecordingEvent = RecordingEvent
    { recordingTimestampMicros :: Int64
    , recordingDirection :: RecordingDirection
    , recordingLine :: Text
    }
    deriving (Eq, Show)

instance ToJSON RecordingEvent where
    toJSON event =
        object
            [ "timestampMicros" .= recordingTimestampMicros event
            , "direction" .= recordingDirection event
            , "line" .= recordingLine event
            ]

instance FromJSON RecordingEvent where
    parseJSON =
        withObject "RecordingEvent" $ \o ->
            RecordingEvent
                <$> o .: "timestampMicros"
                <*> o .: "direction"
                <*> o .: "line"

-- | Replay speed used by @streamReplayRequests@ and @streamReplayFrames@.
data ReplaySpeed
    = ReplayAsFastAsPossible
    | ReplayRealTime
    deriving (Eq, Show, Read)

-- | Handle wrapper for an active JSONL recording file.
data RecordingHandle = RecordingHandle
    { recordingPath :: FilePath
    , recordingOutputHandle :: Handle
    }

data RecordingParseError = RecordingParseError FilePath String
    deriving (Show)

instance Exception RecordingParseError

-- | Open or create a JSONL recording file in append mode.
openRecording :: FilePath -> IO RecordingHandle
openRecording path = do
    createDirectoryIfMissing True (takeDirectory path)
    handle <- openFile path AppendMode
    pure
        RecordingHandle
            { recordingPath = path
            , recordingOutputHandle = handle
            }

-- | Close an active recording handle.
closeRecording :: RecordingHandle -> IO ()
closeRecording = hClose . recordingOutputHandle

-- | Append one JSON-RPC line event to a recording file.
appendRecordingEvent :: RecordingHandle -> RecordingDirection -> Text -> IO ()
appendRecordingEvent handle direction lineValue = do
    micros <- nowMicros
    let event =
            RecordingEvent
                { recordingTimestampMicros = micros
                , recordingDirection = direction
                , recordingLine = lineValue
                }
    hPutStrLn (recordingOutputHandle handle) (toJsonLine event)
    hFlush (recordingOutputHandle handle)

{- | Stream replay of request events directly from a JSONL recording file,
avoiding loading all events into memory at once. Each request line is
parsed and dispatched to the callback one at a time. Returns the total
number of request events replayed.
-}
streamReplayRequests :: ReplaySpeed -> FilePath -> (Text -> IO ()) -> IO Int
streamReplayRequests speed path runRequest =
    streamFilteredEvents speed path DirectionRequest runRequest

{- | Stream replay of frame events from a JSONL recording file. Each
captured viewport frame is dispatched to the callback with inter-frame
timing preserved in real-time mode. Returns the total number of frames
replayed.
-}
streamReplayFrames :: ReplaySpeed -> FilePath -> (Text -> IO ()) -> IO Int
streamReplayFrames speed path showFrame =
    streamFilteredEvents speed path DirectionFrame showFrame

-- | Internal: stream events matching a given direction from a JSONL file.
streamFilteredEvents :: ReplaySpeed -> FilePath -> RecordingDirection -> (Text -> IO ()) -> IO Int
streamFilteredEvents speed path direction callback =
    bracket (openFile path ReadMode) hClose $ \handle ->
        go handle (1 :: Int) Nothing 0
  where
    go handle lineNumber maybePrev !count = do
        eof <- hIsEOF handle
        if eof
            then pure count
            else do
                lineValue <- BS8.hGetLine handle
                let trimmed = TE.decodeUtf8 lineValue
                if T.null (T.strip trimmed)
                    then go handle (lineNumber + 1) maybePrev count
                    else case eitherDecodeStrict' lineValue of
                        Left err ->
                            throwIO
                                (RecordingParseError path ("line " <> show lineNumber <> ": " <> err))
                        Right event
                            | recordingDirection event == direction -> do
                                case speed of
                                    ReplayAsFastAsPossible -> pure ()
                                    ReplayRealTime ->
                                        case maybePrev of
                                            Nothing -> pure ()
                                            Just prev -> do
                                                let delta = recordingTimestampMicros event - recordingTimestampMicros prev
                                                when (delta > 0) $ threadDelay (fromIntegral delta)
                                callback (recordingLine event)
                                go handle (lineNumber + 1) (Just event) (count + 1)
                            | otherwise ->
                                go handle (lineNumber + 1) maybePrev count

toJsonLine :: RecordingEvent -> String
toJsonLine = T.unpack . TE.decodeUtf8 . BL.toStrict . encode

nowMicros :: IO Int64
nowMicros = do
    now <- getPOSIXTime
    pure (floor (now * 1000000))
