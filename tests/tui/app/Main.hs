{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick.AttrMap (AttrMap, attrMap, attrName)
import Brick.Main (App(..), defaultMain, halt)
import Brick.Types (BrickEvent(..), CursorLocation, EventM, Widget)
import Brick.Util (on)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (padAll, str, withAttr)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, modify)
import qualified Graphics.Vty as V
import System.IO (hPutStrLn, stderr)

data Name = CounterWidget
  deriving (Eq, Ord, Show)

data Page
  = CounterPage
  | ChecklistPage
  deriving (Eq, Show)

data Theme
  = Ocean
  | Amber
  | Mono
  deriving (Eq, Show, Enum, Bounded)

data Task = Task
  { taskLabel :: String
  , taskDone :: Bool
  }
  deriving (Eq, Show)

data St = St
  { counter :: Int
  , activePage :: Page
  , activeTheme :: Theme
  , showHelpOverlay :: Bool
  , checklistCursor :: Int
  , checklistTasks :: [Task]
  }

main :: IO ()
main = do
  _ <- defaultMain app initialState
  pure ()

initialState :: St
initialState =
  St
    { counter = 0
    , activePage = CounterPage
    , activeTheme = Ocean
    , showHelpOverlay = False
    , checklistCursor = 0
    , checklistTasks =
        [ Task "smoke assertions" False
        , Task "keyboard flows" False
        , Task "snapshot coverage" False
        ]
    }

app :: App St e Name
app =
  App
    { appDraw = drawUi
    , appChooseCursor = chooseCursor
    , appHandleEvent = handleEvent
    , appStartEvent = emitStateSummary
    , appAttrMap = attributes
    }

drawUi :: St -> [Widget Name]
drawUi stateValue =
  [ center $
      borderWithLabel (str "TuiSpec Brick Showcase") $
        padAll 1 $
        withAttr (attrName "body") $
          str $
            unlines
              ( [ "Page: " <> renderPage (activePage stateValue)
                , "Theme: " <> renderTheme (activeTheme stateValue)
                , "Help: " <> boolText (showHelpOverlay stateValue)
                , "Global keys: n/p page, t theme, h help, q quit"
                , ""
                ]
                  <> pageLines stateValue
                  <> helpLines stateValue
              )
  ]
 where
  pageLines st =
    case activePage st of
      CounterPage ->
        [ "Counter controls: + increment, - decrement, r reset"
        , "Counter: " <> show (counter st)
        ]
      ChecklistPage ->
        [ "Checklist controls: j/k move, space toggle"
        , ""
        ]
          <> renderChecklist st

  renderChecklist st =
    zipWith renderTask [0 :: Int ..] (checklistTasks st)
   where
    renderTask idx taskValue =
      cursorMark idx
        <> " "
        <> checkbox taskValue
        <> " "
        <> taskLabel taskValue

    cursorMark idx
      | idx == checklistCursor st = ">"
      | otherwise = " "

    checkbox taskValue
      | taskDone taskValue = "[x]"
      | otherwise = "[ ]"

  helpLines st
    | showHelpOverlay st =
        [ ""
        , "Help overlay:"
        , "  Counter page lets you bump/reset the counter."
        , "  Checklist page lets you mark progress tasks."
        ]
    | otherwise = []

  boolText True = "on"
  boolText False = "off"

  renderPage CounterPage = "Counter"
  renderPage ChecklistPage = "Checklist"

  renderTheme Ocean = "Ocean"
  renderTheme Amber = "Amber"
  renderTheme Mono = "Mono"

handleEvent :: BrickEvent Name e -> EventM Name St ()
handleEvent eventValue =
  case eventValue of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
    VtyEvent (V.EvKey (V.KChar 'n') []) -> modifyAndEmit (\st -> st {activePage = nextPage (activePage st)})
    VtyEvent (V.EvKey (V.KChar 'p') []) -> modifyAndEmit (\st -> st {activePage = prevPage (activePage st)})
    VtyEvent (V.EvKey (V.KChar 't') []) -> modifyAndEmit (\st -> st {activeTheme = nextTheme (activeTheme st)})
    VtyEvent (V.EvKey (V.KChar 'h') []) -> modifyAndEmit (\st -> st {showHelpOverlay = not (showHelpOverlay st)})
    VtyEvent (V.EvKey (V.KChar '+') [])
      -> modifyWhenCounterPageAndEmit (\st -> st {counter = counter st + 1})
    VtyEvent (V.EvKey (V.KChar '-') [])
      -> modifyWhenCounterPageAndEmit (\st -> st {counter = counter st - 1})
    VtyEvent (V.EvKey (V.KChar 'r') [])
      -> modifyWhenCounterPageAndEmit (\st -> st {counter = 0})
    VtyEvent (V.EvKey (V.KChar 'j') [])
      -> modifyWhenChecklistPageAndEmit (\st -> st {checklistCursor = moveCursor (length (checklistTasks st)) (checklistCursor st) 1})
    VtyEvent (V.EvKey (V.KChar 'k') [])
      -> modifyWhenChecklistPageAndEmit (\st -> st {checklistCursor = moveCursor (length (checklistTasks st)) (checklistCursor st) (-1)})
    VtyEvent (V.EvKey (V.KChar ' ') [])
      -> modifyWhenChecklistPageAndEmit (\st -> st {checklistTasks = toggleTaskAt (checklistCursor st) (checklistTasks st)})
    _ -> pure ()
 where
  modifyAndEmit updateFn = do
    modify updateFn
    emitStateSummary

  modifyWhenCounterPageAndEmit updateFn = do
    modify $ \st ->
      if activePage st == CounterPage
        then updateFn st
        else st
    emitStateSummary

  modifyWhenChecklistPageAndEmit updateFn = do
    modify $ \st ->
      if activePage st == ChecklistPage
        then updateFn st
        else st
    emitStateSummary

nextPage :: Page -> Page
nextPage CounterPage = ChecklistPage
nextPage ChecklistPage = CounterPage

prevPage :: Page -> Page
prevPage CounterPage = ChecklistPage
prevPage ChecklistPage = CounterPage

nextTheme :: Theme -> Theme
nextTheme Ocean = Amber
nextTheme Amber = Mono
nextTheme Mono = Ocean

moveCursor :: Int -> Int -> Int -> Int
moveCursor total current delta
  | total <= 0 = 0
  | otherwise = max 0 (min (total - 1) (current + delta))

toggleTaskAt :: Int -> [Task] -> [Task]
toggleTaskAt targetIndex tasks =
  zipWith toggleOne [0 :: Int ..] tasks
 where
  toggleOne idx taskValue
    | idx == targetIndex = taskValue {taskDone = not (taskDone taskValue)}
    | otherwise = taskValue

chooseCursor :: St -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

attributes :: St -> AttrMap
attributes st =
  attrMap V.defAttr
    [ (attrName "body", foreground `on` background)
    ]
 where
  (foreground, background) =
    case activeTheme st of
      Ocean -> (V.white, V.blue)
      Amber -> (V.black, V.yellow)
      Mono -> (V.white, V.black)

emitStateSummary :: EventM Name St ()
emitStateSummary = do
  st <- get
  liftIO (hPutStrLn stderr "STATE SNAPSHOT")
  mapM_ (liftIO . hPutStrLn stderr) (renderStateSummary st)

renderStateSummary :: St -> [String]
renderStateSummary st =
  [ "STATE page=" <> renderPage (activePage st)
      <> " theme="
      <> renderTheme (activeTheme st)
      <> " help="
      <> boolText (showHelpOverlay st)
      <> " counter="
      <> show (counter st)
  , "STATE cursor=" <> show (checklistCursor st)
  ]
    <> zipWith renderTask [0 :: Int ..] (checklistTasks st)
 where
  boolText True = "on"
  boolText False = "off"

  renderPage CounterPage = "Counter"
  renderPage ChecklistPage = "Checklist"

  renderTheme Ocean = "Ocean"
  renderTheme Amber = "Amber"
  renderTheme Mono = "Mono"

  renderTask idx taskValue =
    "STATE task"
      <> show idx
      <> "="
      <> checkbox taskValue
      <> " "
      <> taskLabel taskValue

  checkbox taskValue
    | taskDone taskValue = "[x]"
    | otherwise = "[ ]"
