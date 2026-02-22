{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TuiSpec.Types (
    AmbiguityMode (..),
    App (..),
    Key (..),
    Modifier (..),
    PtyHandle (..),
    Rect (..),
    RunOptions (..),
    Selector (..),
    SnapshotName (..),
    Spec (..),
    StepOptions (..),
    Tui (..),
    TuiState (..),
    Viewport (..),
    WaitOptions (..),
    defaultRunOptions,
    defaultStepOptions,
    defaultWaitOptions,
) where

import Data.IORef (IORef)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import System.Posix.Pty (Pty)
import System.Process (ProcessHandle)

data Spec = Spec
    { specName :: String
    , specBody :: Tui -> IO ()
    }

data App = App
    { command :: FilePath
    , args :: [String]
    }
    deriving (Eq, Show)

data AmbiguityMode
    = FailOnAmbiguous
    | FirstVisibleMatch
    deriving (Eq, Show, Read)

data RunOptions = RunOptions
    { timeoutSeconds :: Int
    , retries :: Int
    , stepRetries :: Int
    , terminalCols :: Int
    , terminalRows :: Int
    , artifactsDir :: FilePath
    , ambiguityMode :: AmbiguityMode
    , updateSnapshots :: Bool
    , snapshotTheme :: String
    }
    deriving (Eq, Show)

defaultRunOptions :: RunOptions
defaultRunOptions =
    RunOptions
        { timeoutSeconds = 5
        , retries = 0
        , stepRetries = 0
        , terminalCols = 134
        , terminalRows = 40
        , artifactsDir = "artifacts"
        , ambiguityMode = FailOnAmbiguous
        , updateSnapshots = False
        , snapshotTheme = "auto"
        }

data Modifier
    = Control
    | AltModifier
    | Shift
    deriving (Eq, Show, Read)

data Key
    = Enter
    | Esc
    | Tab
    | Backspace
    | ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | Ctrl Char
    | AltKey Char
    | FunctionKey Int
    | CharKey Char
    | NamedKey Text
    deriving (Eq, Show, Read)

data Rect = Rect
    { rectCol :: Int
    , rectRow :: Int
    , rectWidth :: Int
    , rectHeight :: Int
    }
    deriving (Eq, Show, Read)

data Selector
    = Exact Text
    | Regex Text
    | At Int Int
    | Within Rect Selector
    | Nth Int Selector
    deriving (Eq, Show, Read)

data WaitOptions = WaitOptions
    { timeoutMs :: Int
    , pollIntervalMs :: Int
    }
    deriving (Eq, Show, Read)

defaultWaitOptions :: WaitOptions
defaultWaitOptions =
    WaitOptions
        { timeoutMs = 30000
        , pollIntervalMs = 100
        }

newtype SnapshotName = SnapshotName
    { unSnapshotName :: Text
    }
    deriving (Eq, Ord, Show)

instance IsString SnapshotName where
    fromString = SnapshotName . T.pack

data StepOptions = StepOptions
    { stepMaxRetries :: Int
    , stepRetryDelayMs :: Int
    }
    deriving (Eq, Show, Read)

defaultStepOptions :: StepOptions
defaultStepOptions =
    StepOptions
        { stepMaxRetries = 0
        , stepRetryDelayMs = 0
        }

data Viewport = Viewport
    { viewportCols :: Int
    , viewportRows :: Int
    , viewportText :: Text
    }
    deriving (Eq, Show)

data PtyHandle = PtyHandle
    { ptyMaster :: Pty
    , ptyProcess :: ProcessHandle
    }

data TuiState = TuiState
    { launchedApp :: Maybe App
    , visibleBuffer :: Text
    , rawBuffer :: Text
    , actionLog :: [Text]
    , snapshotLog :: [Text]
    , runtimeWarnings :: [Text]
    , frameLog :: [Text]
    }

data Tui = Tui
    { tuiName :: String
    , tuiOptions :: RunOptions
    , tuiTestRoot :: FilePath
    , tuiSnapshotRoot :: FilePath
    , tuiPty :: IORef (Maybe PtyHandle)
    , tuiStateRef :: IORef TuiState
    }
