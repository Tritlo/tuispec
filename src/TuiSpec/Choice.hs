{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TuiSpec.Choice
Description : Helpers for choosing numbered options in terminal prompts.

This module handles a common TUI pattern: a prompt/header appears, followed by
numbered choices such as @1) Haskell@ and @2) Python@. The helpers do not assume
that the choices are rendered as a modal, menu, palette, or any other widget.
-}
module TuiSpec.Choice (
    ChoiceSelectionError (..),
    NumberedChoice (..),
    parseNumberedChoices,
    selectNumberedChoice,
    selectNumberedChoiceWith,
    trySelectNumberedChoice,
    trySelectNumberedChoiceWith,
) where

import Control.Exception (Exception (displayException), SomeException, throwIO, try)
import Control.Monad (replicateM_, unless, when)
import Data.Char (intToDigit, isDigit)
import Data.List (find, findIndex)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)
import TuiSpec.Runner (currentView, defaultWaitOptionsFor, press, waitForSelector)
import TuiSpec.Types (Key (..), Selector (..), Tui, WaitOptions)

-- | One numbered choice parsed from the visible viewport.
data NumberedChoice = NumberedChoice
    { numberedChoiceNumber :: Int
    , numberedChoiceLabel :: Text
    , numberedChoiceSelected :: Bool
    }
    deriving (Eq, Show)

-- | Errors raised by 'selectNumberedChoice' when the prompt or choice cannot be located.
data ChoiceSelectionError
    = -- | The prompt text never appeared before the wait timeout.
      ChoicePromptTimedOut Text
    | -- | The prompt was visible but did not contain the requested choice.
      ChoiceNotFound Text Text [Text]
    | -- | The prompt was visible but had no selected marker for arrow-key fallback.
      ChoiceNotSelectable Text Text [Text]
    deriving (Eq, Show)

instance Exception ChoiceSelectionError where
    displayException err =
        case err of
            ChoicePromptTimedOut promptText ->
                "numbered choice prompt did not appear before timeout: "
                    <> Text.unpack promptText
            ChoiceNotFound promptText choiceText choices ->
                "numbered choice prompt '"
                    <> Text.unpack promptText
                    <> "' did not contain choice '"
                    <> Text.unpack choiceText
                    <> "'. Choices: "
                    <> show choices
            ChoiceNotSelectable promptText choiceText choices ->
                "numbered choice prompt '"
                    <> Text.unpack promptText
                    <> "' contained choice '"
                    <> Text.unpack choiceText
                    <> "' but no selected choice marker was visible for arrow-key fallback. Choices: "
                    <> show choices

-- | Select a numbered choice after its prompt appears, using test defaults.
selectNumberedChoice :: Tui -> Text -> Text -> IO ()
selectNumberedChoice tui =
    selectNumberedChoiceWith tui (defaultWaitOptionsFor tui)

-- | Select a numbered choice after its prompt appears, using explicit wait options.
selectNumberedChoiceWith :: Tui -> WaitOptions -> Text -> Text -> IO ()
selectNumberedChoiceWith tui waitOptions promptText choiceText = do
    promptVisible <- waitForChoicePrompt tui waitOptions promptText
    unless promptVisible $
        throwIO (ChoicePromptTimedOut promptText)
    selectVisibleNumberedChoice tui promptText choiceText

-- | Try to select a numbered choice. Returns 'False' if the prompt never appears.
trySelectNumberedChoice :: Tui -> Text -> Text -> IO Bool
trySelectNumberedChoice tui =
    trySelectNumberedChoiceWith tui (defaultWaitOptionsFor tui)

-- | Try to select a numbered choice with explicit wait options.
trySelectNumberedChoiceWith :: Tui -> WaitOptions -> Text -> Text -> IO Bool
trySelectNumberedChoiceWith tui waitOptions promptText choiceText = do
    promptVisible <- waitForChoicePrompt tui waitOptions promptText
    if promptVisible
        then do
            selectVisibleNumberedChoice tui promptText choiceText
            pure True
        else pure False

waitForChoicePrompt :: Tui -> WaitOptions -> Text -> IO Bool
waitForChoicePrompt tui waitOptions promptText = do
    result <- try (waitForSelector tui waitOptions (Exact promptText)) :: IO (Either SomeException ())
    pure (either (const False) (const True) result)

selectVisibleNumberedChoice :: Tui -> Text -> Text -> IO ()
selectVisibleNumberedChoice tui promptText choiceText = do
    viewport <- currentView tui
    let choices = parseNumberedChoices viewport
    case find (matchesNumberedChoice choiceText) choices of
        Just choice
            | numberedChoiceNumber choice >= 1 && numberedChoiceNumber choice <= 9 -> do
                press tui (CharKey (intToDigit (numberedChoiceNumber choice)))
                viewAfterDigit <- currentView tui
                whenPromptStillVisible promptText viewAfterDigit $
                    selectNumberedChoiceByArrows tui promptText choiceText choices
            | otherwise ->
                selectNumberedChoiceByArrows tui promptText choiceText choices
        Nothing ->
            throwIO (ChoiceNotFound promptText choiceText (map numberedChoiceLabel choices))

whenPromptStillVisible :: Text -> Text -> IO () -> IO ()
whenPromptStillVisible promptText viewport =
    when (promptText `Text.isInfixOf` viewport)

selectNumberedChoiceByArrows :: Tui -> Text -> Text -> [NumberedChoice] -> IO ()
selectNumberedChoiceByArrows tui promptText choiceText choices = do
    let maybeTargetIndex = findIndex (matchesNumberedChoice choiceText) choices
        maybeSelectedIndex = findIndex numberedChoiceSelected choices
    case (maybeTargetIndex, maybeSelectedIndex) of
        (Just targetIndex, Just selectedIndex) -> do
            let delta = targetIndex - selectedIndex
                key = if delta >= 0 then ArrowDown else ArrowUp
            replicateM_ (abs delta) (press tui key)
            press tui Enter
        _ ->
            throwIO (ChoiceNotSelectable promptText choiceText (map numberedChoiceLabel choices))

matchesNumberedChoice :: Text -> NumberedChoice -> Bool
matchesNumberedChoice choiceText choice =
    Text.toLower choiceText `Text.isInfixOf` Text.toLower (numberedChoiceLabel choice)

-- | Parse numbered choices from rendered viewport text.
parseNumberedChoices :: Text -> [NumberedChoice]
parseNumberedChoices =
    mapMaybe parseLine . Text.lines
  where
    parseLine line =
        listToMaybe (mapMaybe parseCandidate (Text.tails line))

    parseCandidate candidate = do
        let stripped = Text.dropWhile isLeftPad candidate
        let (selected, body) =
                case Text.stripPrefix ">" stripped of
                    Just rest -> (True, Text.dropWhile isLeftPad rest)
                    Nothing -> (False, stripped)
        let (digitsText, afterDigits) = Text.span isDigit body
        if Text.null digitsText || not (")" `Text.isPrefixOf` afterDigits)
            then Nothing
            else do
                choiceNumber <- readMaybe (Text.unpack digitsText)
                let label =
                        Text.strip $
                            Text.takeWhile (not . isChoiceBorder) $
                                Text.drop 1 afterDigits
                if Text.null label
                    then Nothing
                    else
                        Just
                            NumberedChoice
                                { numberedChoiceNumber = choiceNumber
                                , numberedChoiceLabel = label
                                , numberedChoiceSelected = selected
                                }

    isLeftPad c = c == ' ' || c == '\t' || isChoiceBorder c

isChoiceBorder :: Char -> Bool
isChoiceBorder char = char `elem` ("│┃║|─━═┌┐└┘┏┓┗┛╔╗╚╝├┤┣┫" :: String)
