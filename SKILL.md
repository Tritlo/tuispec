# REPL Workflow Skill for `tuispec`

This skill explains how to use `tuispec` as a REPL-like driver for TUIs:

1. launch a TUI
2. send commands/keys
3. periodically dump current view snapshots
4. render those snapshots as text/PNG

## Viewing snapshots — prefer PNG

**Always render snapshots to PNG and view the image file** rather than
reading the plain-text `.txt` or `.ansi.txt` representation. A single PNG
gives you the complete visual state of the TUI (layout, colors, borders)
while consuming far less context than the equivalent text grid (a 160×50
viewport is ~8 000 characters of text but a single image token as PNG).

Workflow:

```bash
# Render a snapshot to PNG (output defaults to same path with .png extension)
cabal run tuispec -- render path/to/snapshot.ansi.txt

# Then view the PNG (use your image-reading tool / Read tool)
```

When using the JSON-RPC server, request `"format":"png"` or
`"format":"both"` in `dumpView` so the PNG is generated in one step.
Then read the returned `pngPath` instead of the text path.

Only fall back to `render-text` or `currentView` text when you need to
do programmatic string matching (e.g. searching for a selector or
extracting a specific value from the viewport).

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
  and can render PNG in one call via server (`format: "png"|"both"`)

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
            launch tui (app "sh" ["-lc", "tui-demo"])

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

Render to PNG (preferred — compact, full-fidelity):

```bash
cabal run tuispec -- render artifacts/repl/sessions/demo-session/snapshots/01-board.ansi.txt
# Then read the .png file to see the TUI state
```

Render to visible plain text (only when you need string matching):

```bash
cabal run tuispec -- render-text artifacts/repl/sessions/demo-session/snapshots/01-board.ansi.txt
```

Both commands auto-read rows/cols from `.meta.json`.

## Practical loop

1. run your REPL script
2. render snapshots to PNG and view the images
3. tweak interactions
4. run again

`withTuiSession` resets the session output directory each run, keeping artifact output simple and deterministic.

## JSONRPC server workflow

You can also orchestrate a TUI from an external client using the JSON-RPC
server. See [`SERVER.md`](SERVER.md) for the full protocol reference
(all methods, params, result shapes, error codes, and selector encoding).

```bash
cabal run tuispec -- server --artifact-dir artifacts/server
```

### Request format

- Send one JSON-RPC request per line on stdin.
- Read one JSON-RPC response per line on stdout.

### Example session

Start server:

```bash
cabal run tuispec -- server --artifact-dir artifacts/server
```

Then send requests like:

```json
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"name":"demo"}}
{"jsonrpc":"2.0","id":2,"method":"launch","params":{"command":"sh","args":["-lc","printf 'READY\\n'; exec sh"],"env":{"DEMO_MODE":"1","CLAUDECODE":null},"cwd":".","readySelector":{"type":"exact","text":"READY"}}}
{"jsonrpc":"2.0","id":3,"method":"sendKey","params":{"key":"ArrowDown"}}
{"jsonrpc":"2.0","id":4,"method":"sendKey","params":{"key":"Enter"}}
{"jsonrpc":"2.0","id":5,"method":"dumpView","params":{"name":"after-enter","format":"both"}}
{"jsonrpc":"2.0","id":6,"method":"currentView","params":{"entireRow":1}}
{"jsonrpc":"2.0","id":7,"method":"recording.start","params":{"path":"artifacts/server/demo.jsonl"}}
{"jsonrpc":"2.0","id":8,"method":"recording.stop","params":{}}
{"jsonrpc":"2.0","id":9,"method":"server.shutdown","params":null}
```

### Supported key strings for `sendKey`

- `Enter`, `Esc`, `Tab`, `Backspace`
- `ArrowUp`, `ArrowDown`, `ArrowLeft`, `ArrowRight`
- `F1`..`F12`
- `Ctrl+X`, `Alt+X`, `Shift+X`
- single character like `"a"`

### Dump locations

For `initialize` name `demo`:

- `artifacts/server/sessions/demo/snapshots/<name>.ansi.txt`
- `artifacts/server/sessions/demo/snapshots/<name>.meta.json`

### Rendering dumped views

```bash
cabal run tuispec -- render artifacts/server/sessions/demo/snapshots/after-enter.ansi.txt
cabal run tuispec -- render-text artifacts/server/sessions/demo/snapshots/after-enter.ansi.txt
```

### JSONL recording with viewport frames

Start recording with automatic viewport sampling (default 5 Hz):

```json
{"jsonrpc":"2.0","id":7,"method":"recording.start","params":{"path":"artifacts/server/demo.jsonl"}}
```

To change the sampling rate, pass `frameIntervalMs` (e.g. 100 for 10 Hz, 0 to
disable frame capture):

```json
{"jsonrpc":"2.0","id":7,"method":"recording.start","params":{"path":"artifacts/server/demo.jsonl","frameIntervalMs":100}}
```

### JSONL replay

Replay a recording visually on your terminal (frame-by-frame):

```bash
cabal run tuispec -- replay artifacts/server/demo.jsonl --speed real-time
```

If the recording contains viewport frames, the replay renders each frame in
place with original timing. If no frames are present (older recordings), it
falls back to printing the raw JSON-RPC request lines.

Use `--speed as-fast-as-possible` to skip timing delays.

To show the last input action below the viewport during replay:

```bash
cabal run tuispec -- replay artifacts/server/demo.jsonl --show-input
```

For push-based polling alternatives, subscribe to view changes:

```json
{"jsonrpc":"2.0","id":10,"method":"viewSubscribe","params":{"debounceMs":100,"includeText":false}}
```
