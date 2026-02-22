{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TuiSpec.Types
  ( AmbiguityMode(..)
  , App(..)
  , Key(..)
  , Modifier(..)
  , Rect(..)
  , RunOptions(..)
  , Selector(..)
  , SnapshotName(..)
  , SnapshotSource(..)
  , Spec(..)
  , StepOptions(..)
  , Tui(..)
  , TuiState(..)
  , Viewport(..)
  , WaitOptions(..)
  , defaultRunOptions
  , defaultStepOptions
  , defaultWaitOptions
  ) where

import Data.IORef (IORef)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T

data Spec = Spec
  { specName :: String
  , specBody :: Tui -> IO ()
  }

data App = App
  { command :: FilePath
  , args :: [String]
  }
  deriving (Eq, Show)

data SnapshotSource
  = SnapshotFromTmux
  | SnapshotFromAsciinema
  deriving (Eq, Show, Read)

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
  , snapshotSource :: SnapshotSource
  , ambiguityMode :: AmbiguityMode
  , updateSnapshots :: Bool
  }
  deriving (Eq, Show)

defaultRunOptions :: RunOptions
defaultRunOptions =
  RunOptions
    { timeoutSeconds = 5
    , retries = 0
    , stepRetries = 0
    , terminalCols = 120
    , terminalRows = 40
    , artifactsDir = "artifacts"
    , snapshotSource = SnapshotFromTmux
    , ambiguityMode = FailOnAmbiguous
    , updateSnapshots = False
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
    { timeoutMs = 5000
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

data TuiState = TuiState
  { launchedApp :: Maybe App
  , visibleBuffer :: Text
  , actionLog :: [Text]
  }

data Tui = Tui
  { tuiName :: String
  , tuiOptions :: RunOptions
  , tuiStateRef :: IORef TuiState
  }
