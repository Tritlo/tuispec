{- |
Module      : TuiSpec
Description : Public API for black-box TUI testing over PTY.

This is the primary entry point for test programs. Import this module
to get the full testing DSL:

@
import TuiSpec

main :: IO ()
main =
  defaultMain $ testGroup "demo"
    [ tuiTest defaultRunOptions "my test" $ \\tui -> do
        launch tui (app "my-app" [])
        waitForText tui (Exact "Ready")
        press tui Enter
    ]
@

For advanced integrations (JSON-RPC server, session management), import
"TuiSpec.Runner" and "TuiSpec.Types" directly.
-}
module TuiSpec (
    -- * Test entry point
    tuiTest,
    withTuiSession,

    -- * Actions
    launch,
    press,
    pressCombo,
    typeText,
    sendLine,

    -- * Waits and assertions
    waitFor,
    waitForStable,
    waitForText,
    waitForSelector,
    expectVisible,
    expectNotVisible,

    -- * Snapshots
    expectSnapshot,
    dumpView,
    currentView,

    -- * Steps
    step,

    -- * Rendering
    renderAnsiViewportText,
    renderAnsiSnapshotTextFile,

    -- * Configuration types
    RunOptions (..),
    defaultRunOptions,
    defaultWaitOptions,
    defaultWaitOptionsFor,
    App (..),
    app,
    Key (..),
    Modifier (..),
    Selector (..),
    Rect (..),
    SnapshotName (..),
    AmbiguityMode (..),
    WaitOptions (..),
    StepOptions (..),
    defaultStepOptions,
    Viewport (..),

    -- * Runtime handle (opaque)
    Tui,
) where

import TuiSpec.Render (renderAnsiSnapshotTextFile)
import TuiSpec.Runner
import TuiSpec.Types
