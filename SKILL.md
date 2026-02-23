# REPL Workflow Skill for `tuispec`

This skill explains how to use `tuispec` as a REPL-like driver for TUIs:

1. launch a TUI
2. send commands/keys
3. periodically dump current view snapshots
4. render those snapshots as text/PNG

## What was added for REPL ergonomics

Use these APIs from `TuiSpec`:

- `withTuiSession`:
  creates an ad-hoc PTY session outside `tasty`
- `sendLine`:
  sends text + `Enter`
- `currentView`:
  returns current visible viewport text
- `dumpView`:
  writes the current screen to `<name>.ansi.txt` + `<name>.meta.json`

Relevant implementation:

- `src/TuiSpec/Runner.hs`
- `src/TuiSpec/Render.hs`

## Minimal REPL script

Create a Haskell program (for example `scratch/ReplDemo.hs`):

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import TuiSpec

main :: IO ()
main =
    withTuiSession
        defaultRunOptions
            { artifactsDir = "artifacts/repl"
            , terminalCols = 134
            , terminalRows = 40
            }
        "demo-session"
        $ \tui -> do
            launch tui (App "sh" ["-lc", "tui-demo"])

            _ <- dumpView tui "00-initial"

            press tui (CharKey 'b')
            _ <- dumpView tui "01-board"

            sendLine tui "/help"
            _ <- dumpView tui "02-help"

            view <- currentView tui
            putStrLn "Current viewport:"
            putStrLn (take 1000 (show view))

            -- optional short settle pause between interactions
            threadDelay (150 * 1000)

            press tui (CharKey 'q')
```

Run it:

```bash
cabal run runghc -- -isrc scratch/ReplDemo.hs
```

## Where dumps go

For session name `demo-session` and `artifactsDir = artifacts/repl`:

- `artifacts/repl/sessions/demo-session/snapshots/00-initial.ansi.txt`
- `artifacts/repl/sessions/demo-session/snapshots/00-initial.meta.json`
- `artifacts/repl/sessions/demo-session/snapshots/01-board.ansi.txt`
- `...`

## Render dumps while iterating

Render to PNG:

```bash
cabal run tuispec -- render artifacts/repl/sessions/demo-session/snapshots/01-board.ansi.txt
```

Render to visible plain text:

```bash
cabal run tuispec -- render-text artifacts/repl/sessions/demo-session/snapshots/01-board.ansi.txt
```

Both commands auto-read rows/cols from `.meta.json`.

## Practical loop

1. run your REPL script
2. inspect generated `.txt`/`.png`
3. tweak interactions
4. run again

`withTuiSession` resets the session output directory each run, keeping artifact output simple and deterministic.
