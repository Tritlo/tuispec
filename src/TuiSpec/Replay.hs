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
    computeFrameDelta,
    openRecording,
    streamReplayFrames,
    streamReplayRequests,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Array, Number, String), eitherDecodeStrict', encode, object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
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
    | -- | Full viewport keyframe (line contains the complete visible text).
      DirectionFrame
    | -- | Delta frame (line contains JSON-encoded changed lines since last keyframe).
      DirectionFrameDelta
    deriving (Eq, Show)

instance ToJSON RecordingDirection where
    toJSON value =
        case value of
            DirectionRequest -> toJSON ("request" :: Text)
            DirectionResponse -> toJSON ("response" :: Text)
            DirectionNotification -> toJSON ("notification" :: Text)
            DirectionFrame -> toJSON ("frame" :: Text)
            DirectionFrameDelta -> toJSON ("frame-delta" :: Text)

instance FromJSON RecordingDirection where
    parseJSON value = do
        raw <- parseJSON value
        case (T.toLower (T.strip raw) :: Text) of
            "request" -> pure DirectionRequest
            "response" -> pure DirectionResponse
            "notification" -> pure DirectionNotification
            "frame" -> pure DirectionFrame
            "frame-delta" -> pure DirectionFrameDelta
            _ -> fail "direction must be one of: request, response, notification, frame, frame-delta"

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

{- | Stream replay of frame events from a JSONL recording file. Handles
both full keyframes (@frame@) and delta frames (@frame-delta@),
reconstructing full viewport text before dispatching to the callback.
Returns the total number of frames replayed.
-}
streamReplayFrames :: ReplaySpeed -> FilePath -> (Text -> IO ()) -> IO Int
streamReplayFrames speed path showFrame =
    bracket (openFile path ReadMode) hClose $ \fileHandle -> do
        currentLinesRef <- newIORef ([] :: [Text])
        go fileHandle currentLinesRef (1 :: Int) Nothing 0
  where
    go fileHandle currentLinesRef lineNumber maybePrev !count = do
        eof <- hIsEOF fileHandle
        if eof
            then pure count
            else do
                lineValue <- BS8.hGetLine fileHandle
                let trimmed = TE.decodeUtf8 lineValue
                if T.null (T.strip trimmed)
                    then go fileHandle currentLinesRef (lineNumber + 1) maybePrev count
                    else case eitherDecodeStrict' lineValue of
                        Left err ->
                            throwIO
                                (RecordingParseError path ("line " <> show lineNumber <> ": " <> err))
                        Right event
                            | recordingDirection event == DirectionFrame -> do
                                applyReplayDelay speed maybePrev event
                                let frameLines = T.splitOn "\n" (recordingLine event)
                                writeIORef currentLinesRef frameLines
                                showFrame (recordingLine event)
                                go fileHandle currentLinesRef (lineNumber + 1) (Just event) (count + 1)
                            | recordingDirection event == DirectionFrameDelta -> do
                                applyReplayDelay speed maybePrev event
                                baseLines <- readIORef currentLinesRef
                                let updatedLines = applyDelta baseLines (recordingLine event)
                                writeIORef currentLinesRef updatedLines
                                showFrame (T.intercalate "\n" updatedLines)
                                go fileHandle currentLinesRef (lineNumber + 1) (Just event) (count + 1)
                            | otherwise ->
                                go fileHandle currentLinesRef (lineNumber + 1) maybePrev count

-- | Internal: stream events matching a given direction from a JSONL file.
streamFilteredEvents :: ReplaySpeed -> FilePath -> RecordingDirection -> (Text -> IO ()) -> IO Int
streamFilteredEvents speed path direction callback =
    bracket (openFile path ReadMode) hClose $ \fileHandle ->
        go fileHandle (1 :: Int) Nothing 0
  where
    go fileHandle lineNumber maybePrev !count = do
        eof <- hIsEOF fileHandle
        if eof
            then pure count
            else do
                lineValue <- BS8.hGetLine fileHandle
                let trimmed = TE.decodeUtf8 lineValue
                if T.null (T.strip trimmed)
                    then go fileHandle (lineNumber + 1) maybePrev count
                    else case eitherDecodeStrict' lineValue of
                        Left err ->
                            throwIO
                                (RecordingParseError path ("line " <> show lineNumber <> ": " <> err))
                        Right event
                            | recordingDirection event == direction -> do
                                applyReplayDelay speed maybePrev event
                                callback (recordingLine event)
                                go fileHandle (lineNumber + 1) (Just event) (count + 1)
                            | otherwise ->
                                go fileHandle (lineNumber + 1) maybePrev count

-- | Apply inter-event timing delay when in real-time replay mode.
applyReplayDelay :: ReplaySpeed -> Maybe RecordingEvent -> RecordingEvent -> IO ()
applyReplayDelay speed maybePrev event =
    case speed of
        ReplayAsFastAsPossible -> pure ()
        ReplayRealTime ->
            case maybePrev of
                Nothing -> pure ()
                Just prev -> do
                    let delta = recordingTimestampMicros event - recordingTimestampMicros prev
                    when (delta > 0) $ threadDelay (fromIntegral delta)

{- | Compute a line-level delta between two viewport texts. Returns
@Nothing@ when the frames are identical, or @Just encodedDelta@ with a
JSON-encoded array of @[lineIndex, \"new line text\"]@ pairs for each
changed line.
-}
computeFrameDelta :: Text -> Text -> Maybe Text
computeFrameDelta oldFrame newFrame
    | oldFrame == newFrame = Nothing
    | otherwise =
        let oldLines = T.splitOn "\n" oldFrame
            newLines = T.splitOn "\n" newFrame
            changes = collectChanges 0 oldLines newLines
         in if null changes
                then Nothing
                else Just (encodeDelta changes)

-- | Collect (index, newLine) pairs for lines that differ.
collectChanges :: Int -> [Text] -> [Text] -> [(Int, Text)]
collectChanges !idx olds news =
    case (olds, news) of
        ([], []) -> []
        ([], n : ns) -> (idx, n) : collectChanges (idx + 1) [] ns
        (_ : os, []) -> (idx, "") : collectChanges (idx + 1) os []
        (o : os, n : ns)
            | o == n -> collectChanges (idx + 1) os ns
            | otherwise -> (idx, n) : collectChanges (idx + 1) os ns

-- | Encode a list of (lineIndex, text) changes as a JSON array of pairs.
encodeDelta :: [(Int, Text)] -> Text
encodeDelta changes =
    TE.decodeUtf8 . BL.toStrict . Aeson.encode $
        Array (listToAesonArray (map encodePair changes))
  where
    encodePair (idx, txt) =
        Array (listToAesonArray [Number (fromIntegral idx), String txt])

-- | Apply a JSON-encoded delta to a list of lines.
applyDelta :: [Text] -> Text -> [Text]
applyDelta baseLines deltaText =
    case Aeson.eitherDecodeStrict' (TE.encodeUtf8 deltaText) of
        Left _ -> baseLines
        Right (Array arr) ->
            let patches = foldr parsePatch IM.empty (aesonArrayToList arr)
                baseMap = IM.fromList (zip [0 ..] baseLines)
                maxIdx = if IM.null patches then 0 else fst (IM.findMax patches)
                paddedMap =
                    if maxIdx >= length baseLines
                        then IM.union baseMap (IM.fromList [(i, "") | i <- [length baseLines .. maxIdx]])
                        else baseMap
                merged = IM.union patches paddedMap
             in map snd (IM.toAscList merged)
        Right _ -> baseLines
  where
    parsePatch :: Value -> IntMap Text -> IntMap Text
    parsePatch (Array pair) acc =
        case aesonArrayToList pair of
            [Number n, String txt] -> IM.insert (floor n) txt acc
            _ -> acc
    parsePatch _ acc = acc

-- | Convert a Haskell list to an Aeson Array value.
listToAesonArray :: [Value] -> Aeson.Array
listToAesonArray = foldMap (\v -> pure v)

-- | Convert an Aeson Array to a Haskell list.
aesonArrayToList :: Aeson.Array -> [Value]
aesonArrayToList = foldr (:) []

toJsonLine :: RecordingEvent -> String
toJsonLine = T.unpack . TE.decodeUtf8 . BL.toStrict . encode

nowMicros :: IO Int64
nowMicros = do
    now <- getPOSIXTime
    pure (floor (now * 1000000))
