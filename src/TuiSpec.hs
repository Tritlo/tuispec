{- |
Module      : TuiSpec
Description : Public API for black-box TUI testing over PTY.

This module re-exports the main testing DSL from:

- "TuiSpec.Types" for configuration and selectors
- "TuiSpec.Runner" for test actions and assertions
- "TuiSpec.Render" for post-run artifact rendering helpers
-}
module TuiSpec (
    module TuiSpec.Render,
    module TuiSpec.Runner,
    module TuiSpec.Types,
) where

import TuiSpec.Render
import TuiSpec.Runner
import TuiSpec.Types
