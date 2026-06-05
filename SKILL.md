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

## Other DSL helpers (0.3+)

Beyond the minimal example, the library exposes:

- `launchAndWait tui appSpec readySelector` — launch and wait for a ready
  selector in one call (uses session defaults; see `launchAndWaitWith` for
  explicit `WaitOptions`).
- `haskellApp name action` — launch an inline `IO ()` Haskell action under a
  PTY without needing a separate target executable. Honors `cwd` and `env`
  the same way as `app`.
- `loadEnvFile path`, `prependPathEntry dir` — env-file loading (with
  `$VAR` self-reference expansion) and PATH prepending for the test process.
- `selectNumberedChoice tui prompt choice` (and `trySelect…` /
  `…With WaitOptions`) — select numbered options like `1) Haskell` after a
  prompt appears, falling back to arrow-key navigation if a digit press is
  ignored. Throws `ChoiceSelectionError` on failure.
- `artifactRoot tui`, `artifactFile tui rel`, `writeArtifactFile tui rel txt`
  — locate and write into the per-test artifact directory.
- `click tui col row` / `clickSelector tui selector` — simulate a mouse click
  at a 0-based coordinate or on the element a selector matches (Playwright
  style; honors `ambiguityMode`). Use `clickWith` / `clickSelectorWith` with
  `ClickOptions` to choose the button or the legacy X10 encoding. The target
  app must have mouse tracking enabled (`vty`/Brick apps do once mouse mode is
  on); the default SGR encoding suits any modern TUI.
- `currentViewRect tui rect` — return the viewport text cropped to a region.
- `dumpFailureBundle` / `withFailureBundle` — capture diagnostic state on
  failure (snapshot + viewport + action log + warnings + exit status).
  Failed `tuiTest` runs already write `failure-bundles/failure.txt`
  automatically.
- `writeRecording tui path` — export the captured frame log as JSONL with
  wall-clock timestamps; replay with `tuispec replay PATH`.
- `recordTraceTo = Just "trace.jsonl"` in `RunOptions` — automatic trace
  recording. The runner writes a replayable JSONL trace under the per-test
  artifact directory after every `tuiTest`, regardless of pass/fail. Render
  the resulting artifact with `cabal run tuispec -- replay <path>` (use
  `--speed as-fast-as-possible` for CI inspection).

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

### Simulating mouse clicks with `click`

The `click` method synthesizes a mouse click (button press + release). Target
either a 1-based coordinate or a selector:

```json
{"jsonrpc":"2.0","id":10,"method":"click","params":{"col":12,"row":7}}
{"jsonrpc":"2.0","id":11,"method":"click","params":{"selector":{"type":"exact","text":"OK"}}}
{"jsonrpc":"2.0","id":12,"method":"click","params":{"selector":{"type":"exact","text":"Menu"},"button":"right"}}
```

The target application only reacts if it has enabled mouse tracking
(`vty`/Brick apps do once mouse mode is turned on). The default `sgr` encoding
suits any modern TUI; pass `"encoding":"x10"` only for apps that enable just
legacy `\ESC[?1000h` tracking. As with key input, follow a click with
`waitForStable` before dumping the view.

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
