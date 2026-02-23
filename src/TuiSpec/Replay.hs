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
    readRecordingEvents,
    replayRecordedRequests,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
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

-- | Direction of a recorded JSON-RPC line.
data RecordingDirection
    = DirectionRequest
    | DirectionResponse
    | DirectionNotification
    deriving (Eq, Show)

instance ToJSON RecordingDirection where
    toJSON value =
        case value of
            DirectionRequest -> toJSON ("request" :: Text)
            DirectionResponse -> toJSON ("response" :: Text)
            DirectionNotification -> toJSON ("notification" :: Text)

instance FromJSON RecordingDirection where
    parseJSON value = do
        raw <- parseJSON value
        case (T.toLower (T.strip raw) :: Text) of
            "request" -> pure DirectionRequest
            "response" -> pure DirectionResponse
            "notification" -> pure DirectionNotification
            _ -> fail "direction must be one of: request, response, notification"

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

-- | Replay speed used by @replayRecordedRequests@.
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

-- | Read all events from a JSONL recording file.
readRecordingEvents :: FilePath -> IO [RecordingEvent]
readRecordingEvents path = do
    handle <- openFile path ReadMode
    events <- go handle (1 :: Int) []
    hClose handle
    pure (reverse events)
  where
    go handle lineNumber acc = do
        eof <- hIsEOF handle
        if eof
            then pure acc
            else do
                lineValue <- BS8.hGetLine handle
                let trimmed = TE.decodeUtf8 lineValue
                if T.null (T.strip trimmed)
                    then go handle (lineNumber + 1) acc
                    else case eitherDecodeStrict' lineValue of
                        Left err ->
                            throwIO
                                (RecordingParseError path ("line " <> show lineNumber <> ": " <> err))
                        Right event ->
                            go handle (lineNumber + 1) (event : acc)

-- | Replay recorded request lines, optionally preserving recorded timing.
replayRecordedRequests :: ReplaySpeed -> [RecordingEvent] -> (Text -> IO ()) -> IO Int
replayRecordedRequests replaySpeed events runRequest = do
    let requestEvents = filter ((== DirectionRequest) . recordingDirection) events
    case requestEvents of
        [] -> pure 0
        _ -> do
            _ <- mapM replayOne (zip [0 ..] requestEvents)
            pure (length requestEvents)
  where
    replayOne (idx, event) = do
        maybeDelayMicros <- replayDelayMicros idx event
        case maybeDelayMicros of
            Just micros | micros > 0 -> threadDelay micros
            _ -> pure ()
        runRequest (recordingLine event)

    requestEventsForTiming = filter ((== DirectionRequest) . recordingDirection) events

    replayDelayMicros idx event =
        case replaySpeed of
            ReplayAsFastAsPossible -> pure Nothing
            ReplayRealTime ->
                if idx <= 0
                    then pure Nothing
                    else do
                        let previous = requestEventsForTiming !! (idx - 1)
                        let delta = recordingTimestampMicros event - recordingTimestampMicros previous
                        pure (Just (fromIntegral (max 0 delta)))

toJsonLine :: RecordingEvent -> String
toJsonLine = T.unpack . TE.decodeUtf8 . BL.toStrict . encode

nowMicros :: IO Int64
nowMicros = do
    now <- getPOSIXTime
    pure (floor (now * 1000000))
