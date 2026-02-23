{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : TuiSpec.Types
Description : Core types for TUI test specs, selectors, and runtime options.

These types are the public DSL surface used by test programs.
-}
module TuiSpec.Types (
    AmbiguityMode (..),
    App (..),
    app,
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
    tuispecVersion,
) where

import Data.IORef (IORef)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import System.Posix.Pty (Pty)
import System.Process (ProcessHandle)

-- | A single test specification: name + executable body.
data Spec = Spec
    { specName :: String
    , specBody :: Tui -> IO ()
    }

{- | The command to launch inside the PTY.

`env` overrides inherit from the parent process. Use:

- `Just "value"` to set or override an environment variable
- `Nothing` to unset an inherited variable

`cwd` sets the working directory for the launch command when present.
-}
data App = App
    { command :: FilePath
    , args :: [String]
    , env :: Maybe [(String, Maybe String)]
    , cwd :: Maybe FilePath
    }
    deriving (Eq, Show)

{- | Construct an app launch request using inherited environment variables and
the current working directory.
-}
app :: FilePath -> [String] -> App
app commandValue argsValue =
    App
        { command = commandValue
        , args = argsValue
        , env = Nothing
        , cwd = Nothing
        }

-- | How selector ambiguity is handled for assertion helpers.
data AmbiguityMode
    = -- | Fail when a non-explicit selector matches more than one target.
      FailOnAmbiguous
    | -- | Accept ambiguous matches and use first-match behavior.
      FirstVisibleMatch
    | -- | Accept ambiguous matches and use last-match behavior.
      LastVisibleMatch
    deriving (Eq, Show, Read)

-- | Runtime options for a @tuiTest@ execution.
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

{- | Sensible defaults for local and CI runs.

Defaults:

- timeout: 5s
- retries: 0
- viewport: 134x40
- artifacts dir: @artifacts@
-}
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

-- | The tuispec library/protocol version.
tuispecVersion :: String
tuispecVersion = "0.2.0.0"

-- | Key modifiers for combo key presses.
data Modifier
    = Control
    | Alt
    | Shift
    deriving (Eq, Show, Read)

-- | Input key model used by @press@ and @pressCombo@.
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

-- | A rectangular region within the terminal viewport.
data Rect = Rect
    { rectCol :: Int
    , rectRow :: Int
    , rectWidth :: Int
    , rectHeight :: Int
    }
    deriving (Eq, Show, Read)

-- | Selector language used by visibility and text assertions.
data Selector
    = Exact Text
    | Regex Text
    | At Int Int
    | Within Rect Selector
    | Nth Int Selector
    deriving (Eq, Show, Read)

-- | Polling behavior for @waitFor@.
data WaitOptions = WaitOptions
    { timeoutMs :: Int
    , pollIntervalMs :: Int
    }
    deriving (Eq, Show, Read)

-- | Default wait behavior: 30s timeout with 100ms polling.
defaultWaitOptions :: WaitOptions
defaultWaitOptions =
    WaitOptions
        { timeoutMs = 30000
        , pollIntervalMs = 100
        }

-- | Strongly-typed snapshot identifier.
newtype SnapshotName = SnapshotName
    { unSnapshotName :: Text
    }
    deriving (Eq, Ord, Show)

instance IsString SnapshotName where
    fromString = SnapshotName . T.pack

-- | Retry behavior for a single logical test step.
data StepOptions = StepOptions
    { stepMaxRetries :: Int
    , stepRetryDelayMs :: Int
    }
    deriving (Eq, Show, Read)

-- | Default step retries: no retries, no delay.
defaultStepOptions :: StepOptions
defaultStepOptions =
    StepOptions
        { stepMaxRetries = 0
        , stepRetryDelayMs = 0
        }

-- | The current terminal viewport text plus dimensions.
data Viewport = Viewport
    { viewportCols :: Int
    , viewportRows :: Int
    , viewportText :: Text
    }
    deriving (Eq, Show)

-- | Handle to the child process and PTY master descriptor.
data PtyHandle = PtyHandle
    { ptyMaster :: Pty
    , ptyProcess :: ProcessHandle
    }

-- | Mutable runtime state tracked while a test executes.
data TuiState = TuiState
    { launchedApp :: Maybe App
    , visibleBuffer :: Text
    , rawBuffer :: Text
    , actionLog :: [Text]
    , snapshotLog :: [Text]
    , runtimeWarnings :: [Text]
    , frameLog :: [Text]
    }

{- | Runtime test handle passed into each spec body.

This is intentionally opaque in normal test usage, even though
record fields are exported for advanced integrations.
-}
data Tui = Tui
    { tuiName :: String
    , tuiOptions :: RunOptions
    , tuiRootDir :: FilePath
    , tuiTestRoot :: FilePath
    , tuiSnapshotRoot :: FilePath
    , tuiPty :: IORef (Maybe PtyHandle)
    , tuiStateRef :: IORef TuiState
    }
