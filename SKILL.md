# REPL Workflow Skill for `tuispec`

This skill explains how to use `tuispec` as a REPL-like driver for TUIs:

1. launch a TUI
2. send commands/keys
3. wait for stable output (no more fixed sleeps)
4. dump and view snapshots

## Golden path: wait for stability, not time

The most common mistake in TUI testing is using fixed sleeps (`threadDelay`)
to wait for output. This is inherently flaky — timing varies across machines,
CI providers, and load conditions.

**Always use `waitForStable` instead of `threadDelay`.**

`waitForStable` polls the viewport and returns once the visible text has been
unchanged for a configurable debounce period. This makes tests both faster
(no over-sleeping) and more reliable (no under-sleeping).

## Choosing a wait strategy

| Situation | Use | Why |
|-----------|-----|-----|
| Wait for output to settle after an action | `waitForStable` | No specific text to match; just need rendering to finish |
| Wait for specific text to appear | `waitForText` / `waitUntil` | You know what the TUI should display |
| Assert text is present right now | `expectVisible` | Viewport already settled, just check |
| Never | `threadDelay` / fixed sleep | Flaky by design |

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

## Minimal REPL script (Haskell DSL)

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

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
            let wait = waitForStable tui (defaultWaitOptionsFor tui) 300

            launch tui (app "sh" ["-lc", "tui-demo"])
            wait
            _ <- dumpView tui "00-initial"

            press tui (CharKey 'b')
            wait
            _ <- dumpView tui "01-board"

            sendLine tui "/help"
            wait
            _ <- dumpView tui "02-help"

            press tui (CharKey 'q')
```

Key points:
- `waitForStable tui opts 300` — wait until viewport unchanged for 300ms
- Bind a local `wait` helper so every action is followed by a stability gate
- No `threadDelay` anywhere

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

### Example session with waitForStable

Start server:

```bash
cabal run tuispec -- server --artifact-dir artifacts/server
```

Then send requests:

```json
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"name":"demo"}}
{"jsonrpc":"2.0","id":2,"method":"launch","params":{"command":"sh","args":["-lc","printf 'READY\\n'; exec sh"],"readySelector":{"type":"exact","text":"READY"}}}
{"jsonrpc":"2.0","id":3,"method":"sendKey","params":{"key":"ArrowDown"}}
{"jsonrpc":"2.0","id":4,"method":"waitForStable","params":{"debounceMs":300}}
{"jsonrpc":"2.0","id":5,"method":"dumpView","params":{"name":"after-arrow","format":"both"}}
{"jsonrpc":"2.0","id":6,"method":"sendKey","params":{"key":"Enter"}}
{"jsonrpc":"2.0","id":7,"method":"waitForStable","params":{"debounceMs":300}}
{"jsonrpc":"2.0","id":8,"method":"dumpView","params":{"name":"after-enter","format":"both"}}
{"jsonrpc":"2.0","id":9,"method":"server.shutdown","params":null}
```

Notice: every input action is followed by `waitForStable` before dumping the
view. This replaces fixed sleeps and makes the script reliable across machines.

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

#### Interactive replay controls

When replaying on a TTY, the following keyboard controls are available:

| Key         | Action                          |
|-------------|---------------------------------|
| Space       | Pause / resume playback         |
| Left/Right  | Step one frame backward/forward |
| Up/Down     | Skip 5 frames backward/forward  |
| r           | Redraw current frame            |
| q / Esc     | Quit                            |

For push-based polling alternatives, subscribe to view changes:

```json
{"jsonrpc":"2.0","id":10,"method":"viewSubscribe","params":{"debounceMs":100,"includeText":false}}
```

## Common failure modes and troubleshooting

### `waitForStable` times out

- **Cause**: the TUI keeps updating (clock, spinner, streaming output).
- **Fix**: increase `debounceMs` to tolerate brief pauses in output, or use
  `waitForText`/`waitUntil` with a specific pattern instead.

### `waitForStable` returns too early

- **Cause**: `debounceMs` is too short — the TUI paused briefly mid-render.
- **Fix**: increase `debounceMs` (300ms is a good default; 500ms+ for apps
  with multi-phase rendering).

### Snapshot mismatch after stable wait

- **Cause**: viewport settled on different content than expected (race in app
  startup, wrong initial state).
- **Fix**: use `waitForText` with a readiness selector before taking snapshots.
  Combine: `waitForText` for readiness, then `waitForStable` for full settle.

### Server returns `-32001` (no active session)

- **Cause**: forgot to call `initialize` before other methods.
- **Fix**: always start with `initialize`, then `launch`.

### App exits immediately

- **Cause**: command fails or exits before the test can interact with it.
- **Fix**: verify the command works in a regular terminal first. Check `cwd`
  and `env` params. Use `readySelector` in `launch` to gate on app startup.
