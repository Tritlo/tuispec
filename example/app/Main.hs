{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick.AttrMap (AttrMap, attrMap, attrName)
import Brick.Main (App (..), defaultMain, halt)
import Brick.Types (BrickEvent (..), CursorLocation, EventM, Widget)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Core (
    Padding (..),
    fill,
    hBox,
    hLimitPercent,
    padAll,
    padLeftRight,
    padRight,
    str,
    vBox,
    vLimit,
    vLimitPercent,
    withAttr,
 )
import Control.Monad.State.Strict (get, modify)
import Graphics.Vty qualified as V

data Name = MainUi
    deriving (Eq, Ord, Show)

data Screen
    = Dashboard
    | Board
    | LogsScreen
    deriving (Eq, Show)

data Pane
    = NavPane
    | MainPane
    | InspectPane
    deriving (Eq, Show)

data Split
    = VerticalSplit
    | HorizontalSplit
    deriving (Eq, Show)

data Theme
    = Ocean
    | Amber
    | Mono
    deriving (Eq, Show)

data Task = Task
    { taskLabel :: String
    , taskDone :: Bool
    }
    deriving (Eq, Show)

data St = St
    { screenMode :: Screen
    , focusPane :: Pane
    , splitMode :: Split
    , themeMode :: Theme
    , helpOpen :: Bool
    , commandOpen :: Bool
    , commandInput :: String
    , counter :: Int
    , taskCursor :: Int
    , tasks :: [Task]
    , ticketCursor :: Int
    , eventLog :: [String]
    }

main :: IO ()
main = do
    _ <- defaultMain app initialState
    pure ()

initialState :: St
initialState =
    St
        { screenMode = Dashboard
        , focusPane = MainPane
        , splitMode = VerticalSplit
        , themeMode = Ocean
        , helpOpen = False
        , commandOpen = False
        , commandInput = ""
        , counter = 0
        , taskCursor = 0
        , tasks =
            [ Task "Write selectors for panes" False
            , Task "Exercise split toggle" False
            , Task "Capture snapshot gallery" False
            , Task "Verify retries + isolation" False
            ]
        , ticketCursor = 0
        , eventLog = ["app booted"]
        }

app :: App St e Name
app =
    App
        { appDraw = drawUi
        , appChooseCursor = chooseCursor
        , appHandleEvent = handleEvent
        , appStartEvent = pure ()
        , appAttrMap = attributes
        }

drawUi :: St -> [Widget Name]
drawUi st =
    [ withAttr (attrName "base") $
        vBox
            [ tabBar st
            , statusBar st
            , vLimit 31 (splitContent st)
            , commandBar st
            , helpBlock st
            , fill ' '
            ]
    ]

tabBar :: St -> Widget Name
tabBar st =
    borderWithLabel (str "Screens") $
        hBox
            [ tabLabel st Dashboard "1 dashboard"
            , tabLabel st Board "2 board"
            , tabLabel st LogsScreen "3 logs"
            , fill ' '
            ]

tabLabel :: St -> Screen -> String -> Widget Name
tabLabel st candidate labelText =
    padRight (Pad 1) $
        if screenMode st == candidate
            then withAttr (attrName "tab-active") (str ("[" <> labelText <> "]"))
            else str (" " <> labelText <> " ")

statusBar :: St -> Widget Name
statusBar st =
    borderWithLabel (str "Session") $
        hBox
            [ withAttr (attrName "accent") (str ("focus=" <> renderPane (focusPane st)))
            , padLeftRight 1 (str ("split=" <> renderSplit (splitMode st)))
            , padLeftRight 1 (str ("theme=" <> renderTheme (themeMode st)))
            , padLeftRight 1 (str ("counter=" <> show (counter st)))
            , padLeftRight 1 (str ("ticket=" <> show (ticketCursor st)))
            , fill ' '
            ]

splitContent :: St -> Widget Name
splitContent st =
    case splitMode st of
        VerticalSplit ->
            hBox
                [ hLimitPercent 24 (navPane st)
                , hLimitPercent 52 (mainPane st)
                , hLimitPercent 24 (inspectorPane st)
                ]
        HorizontalSplit ->
            vBox
                [ vLimitPercent 62 $
                    hBox
                        [ hLimitPercent 35 (navPane st)
                        , hLimitPercent 65 (mainPane st)
                        ]
                , vLimitPercent 38 (inspectorPane st)
                ]

navPane :: St -> Widget Name
navPane st =
    paneBox st NavPane "Navigation" $
        vBox
            [ navEntry st Dashboard
            , navEntry st Board
            , navEntry st LogsScreen
            , str ""
            , withAttr (attrName "dim") (str "tab: cycle pane")
            , withAttr (attrName "dim") (str "s: toggle split")
            , withAttr (attrName "dim") (str "?: help")
            ]

mainPane :: St -> Widget Name
mainPane st =
    paneBox st MainPane "Main" $
        case screenMode st of
            Dashboard ->
                vBox
                    [ str "Agent Readiness"
                    , str ("Open tasks: " <> show (length (filter (not . taskDone) (tasks st))))
                    , str ("Done tasks: " <> show (length (filter taskDone (tasks st))))
                    , str "Keys: + / - adjust counter, t theme, / command palette"
                    ]
            Board ->
                vBox
                    ( str "Board Tasks (j/k move, space toggle)"
                        : zipWith (renderTask st) [0 :: Int ..] (tasks st)
                    )
            LogsScreen ->
                vBox (str "Recent Events" : map (str . ("- " <>)) (takeLast 8 (eventLog st)))

inspectorPane :: St -> Widget Name
inspectorPane st =
    paneBox st InspectPane "Inspector" $
        vBox
            [ str ("Current screen: " <> renderScreen (screenMode st))
            , str ("Focused pane: " <> renderPane (focusPane st))
            , str ("Theme: " <> renderTheme (themeMode st))
            , str ("Counter: " <> show (counter st))
            , str ("Ticket cursor: " <> show (ticketCursor st))
            , str ""
            , withAttr (attrName "dim") (str "Arrow left/right: ticket")
            , withAttr (attrName "dim") (str "g/b/l: switch screen")
            , withAttr (attrName "dim") (str "r: reset counter")
            ]

paneBox :: St -> Pane -> String -> Widget Name -> Widget Name
paneBox st paneId labelText inner =
    borderWithLabel (label labelText) $
        padAll 1 (vBox [inner, fill ' '])
  where
    label textValue
        | focusPane st == paneId = withAttr (attrName "pane-active") (str textValue)
        | otherwise = str textValue

navEntry :: St -> Screen -> Widget Name
navEntry st item =
    let marker = if screenMode st == item then ">" else " "
     in str (marker <> " " <> renderScreen item)

renderTask :: St -> Int -> Task -> Widget Name
renderTask st idx taskValue =
    let cursor = if idx == taskCursor st then ">" else " "
        check = if taskDone taskValue then "[x]" else "[ ]"
     in str (cursor <> " " <> check <> " " <> taskLabel taskValue)

commandBar :: St -> Widget Name
commandBar st =
    borderWithLabel (str "Command") $
        if commandOpen st
            then withAttr (attrName "command") (str (":" <> commandInput st <> "_"))
            else withAttr (attrName "dim") (str "Press / to open command mode")

helpBlock :: St -> Widget Name
helpBlock st
    | not (helpOpen st) = str ""
    | otherwise =
        borderWithLabel (str "Help") $
            vBox
                [ str "Global: q quit, tab next pane, s split, t theme, ? help"
                , str "Screens: g dashboard, b board, l logs"
                , str "Board: j/k move, space toggle task"
                , str "Inspector: left/right ticket cursor"
                , str "Command: / open, enter apply, esc close"
                ]

handleEvent :: BrickEvent Name e -> EventM Name St ()
handleEvent eventValue = do
    st <- get
    if commandOpen st
        then handleCommandEvent eventValue
        else handleNormalEvent eventValue

handleNormalEvent :: BrickEvent Name e -> EventM Name St ()
handleNormalEvent eventValue =
    case eventValue of
        VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
        VtyEvent (V.EvKey (V.KChar '\t') []) -> updateAndEmit (\st -> st{focusPane = nextPane (focusPane st)}) "focus cycled"
        VtyEvent (V.EvKey (V.KChar 's') []) -> updateAndEmit (\st -> st{splitMode = toggleSplit (splitMode st)}) "split toggled"
        VtyEvent (V.EvKey (V.KChar 't') []) -> updateAndEmit (\st -> st{themeMode = nextTheme (themeMode st)}) "theme changed"
        VtyEvent (V.EvKey (V.KChar '?') []) -> updateAndEmit (\st -> st{helpOpen = not (helpOpen st)}) "help toggled"
        VtyEvent (V.EvKey (V.KChar '/') []) -> updateAndEmit (\st -> st{commandOpen = True, commandInput = ""}) "command opened"
        VtyEvent (V.EvKey (V.KChar 'g') []) -> updateAndEmit (\st -> st{screenMode = Dashboard}) "screen dashboard"
        VtyEvent (V.EvKey (V.KChar 'b') []) -> updateAndEmit (\st -> st{screenMode = Board}) "screen board"
        VtyEvent (V.EvKey (V.KChar 'l') []) -> updateAndEmit (\st -> st{screenMode = LogsScreen}) "screen logs"
        VtyEvent (V.EvKey (V.KChar '+') []) -> updateAndEmit (\st -> st{counter = counter st + 1}) "counter inc"
        VtyEvent (V.EvKey (V.KChar '-') []) -> updateAndEmit (\st -> st{counter = counter st - 1}) "counter dec"
        VtyEvent (V.EvKey (V.KChar 'r') []) -> updateAndEmit (\st -> st{counter = 0}) "counter reset"
        VtyEvent (V.EvKey V.KLeft []) -> updateAndEmit (\st -> st{ticketCursor = max 0 (ticketCursor st - 1)}) "ticket left"
        VtyEvent (V.EvKey V.KRight []) -> updateAndEmit (\st -> st{ticketCursor = min 9 (ticketCursor st + 1)}) "ticket right"
        VtyEvent (V.EvKey (V.KChar 'j') []) -> updateAndEmit moveDown "cursor down"
        VtyEvent (V.EvKey (V.KChar 'k') []) -> updateAndEmit moveUp "cursor up"
        VtyEvent (V.EvKey (V.KChar ' ') []) -> updateAndEmit toggleCurrentTask "task toggled"
        _ -> pure ()
  where
    moveDown st =
        st
            { taskCursor = min (max 0 (length (tasks st) - 1)) (taskCursor st + 1)
            }

    moveUp st =
        st
            { taskCursor = max 0 (taskCursor st - 1)
            }

handleCommandEvent :: BrickEvent Name e -> EventM Name St ()
handleCommandEvent eventValue =
    case eventValue of
        VtyEvent (V.EvKey V.KEsc []) ->
            updateAndEmit (\st -> st{commandOpen = False, commandInput = ""}) "command cancelled"
        VtyEvent (V.EvKey V.KEnter []) -> do
            st <- get
            if words (commandInput st) == ["quit"]
                then halt
                else modify (applyCommandAndClose (commandInput st))
        VtyEvent (V.EvKey V.KBS []) ->
            updateAndEmit
                (\st -> st{commandInput = if null (commandInput st) then "" else init (commandInput st)})
                "command edit"
        VtyEvent (V.EvKey (V.KChar c) []) ->
            updateAndEmit (\st -> st{commandInput = commandInput st <> [c]}) "command edit"
        _ -> pure ()

applyCommand :: String -> St -> St
applyCommand rawInput st =
    logEvent ("command:" <> rawInput) $
        case words rawInput of
            ["reset"] ->
                st
                    { counter = 0
                    , taskCursor = 0
                    , tasks = map (\t -> t{taskDone = False}) (tasks st)
                    }
            ["screen", "dashboard"] -> st{screenMode = Dashboard}
            ["screen", "board"] -> st{screenMode = Board}
            ["screen", "logs"] -> st{screenMode = LogsScreen}
            ["split", "vertical"] -> st{splitMode = VerticalSplit}
            ["split", "horizontal"] -> st{splitMode = HorizontalSplit}
            ["theme", "ocean"] -> st{themeMode = Ocean}
            ["theme", "amber"] -> st{themeMode = Amber}
            ["theme", "mono"] -> st{themeMode = Mono}
            ["quit"] -> st
            _ -> st

applyCommandAndClose :: String -> St -> St
applyCommandAndClose rawInput st =
    let updatedState = applyCommand rawInput st
     in updatedState{commandOpen = False, commandInput = ""}

toggleCurrentTask :: St -> St
toggleCurrentTask st =
    st
        { tasks = zipWith toggleOne [0 :: Int ..] (tasks st)
        }
  where
    toggleOne idx taskValue
        | idx == taskCursor st = taskValue{taskDone = not (taskDone taskValue)}
        | otherwise = taskValue

updateAndEmit :: (St -> St) -> String -> EventM Name St ()
updateAndEmit updateFn eventName = do
    modify (logEvent eventName . updateFn)

logEvent :: String -> St -> St
logEvent message st =
    st{eventLog = takeLast 18 (eventLog st <> [message])}

nextPane :: Pane -> Pane
nextPane NavPane = MainPane
nextPane MainPane = InspectPane
nextPane InspectPane = NavPane

toggleSplit :: Split -> Split
toggleSplit VerticalSplit = HorizontalSplit
toggleSplit HorizontalSplit = VerticalSplit

nextTheme :: Theme -> Theme
nextTheme Ocean = Amber
nextTheme Amber = Mono
nextTheme Mono = Ocean

chooseCursor :: St -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

attributes :: St -> AttrMap
attributes _ =
    attrMap
        V.defAttr
        [ (attrName "base", V.defAttr)
        , (attrName "tab-active", V.withStyle V.defAttr V.bold)
        , (attrName "pane-active", V.withStyle V.defAttr V.bold)
        , (attrName "accent", V.withStyle V.defAttr V.bold)
        , (attrName "dim", V.withStyle V.defAttr V.dim)
        , (attrName "command", V.defAttr)
        ]

renderScreen :: Screen -> String
renderScreen Dashboard = "dashboard"
renderScreen Board = "board"
renderScreen LogsScreen = "logs"

renderPane :: Pane -> String
renderPane NavPane = "nav"
renderPane MainPane = "main"
renderPane InspectPane = "inspect"

renderSplit :: Split -> String
renderSplit VerticalSplit = "vertical"
renderSplit HorizontalSplit = "horizontal"

renderTheme :: Theme -> String
renderTheme Ocean = "ocean"
renderTheme Amber = "amber"
renderTheme Mono = "mono"

takeLast :: Int -> [a] -> [a]
takeLast count values
    | count <= 0 = []
    | otherwise =
        let total = length values
         in drop (max 0 (total - count)) values
