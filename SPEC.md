# tuispec Specification

Last updated: 2026-02-23
Status: current implementation

## 1. Purpose

`tuispec` is a Haskell framework for black-box testing of terminal UIs (TUIs) over PTY.

Primary goals:
- let developers write reliable TUI tests as normal Haskell programs
- make tests agent-friendly (explicit actions, explicit waits, deterministic artifacts)
- keep transport generic (no framework instrumentation inside the target app)

## 2. Platform and Scope

In scope:
- Linux terminal environments
- PTY-backed app execution
- single viewport per test/session
- text assertions and snapshots (`.ansi.txt` + metadata, optional PNG rendering)
- `tasty` integration (`tuiTest`)
- ad-hoc session mode (`withTuiSession`) for REPL-like workflows
- JSON-RPC server for interactive orchestration (`tuispec server`)

Out of scope:
- Windows/macOS support
- browser/web UI for reports
- multi-pane orchestration
- in-process hooks into TUI frameworks

## 3. Core Model

Main public modules:
- `TuiSpec`
- `TuiSpec.Types`
- `TuiSpec.Runner`
- `TuiSpec.Render`

Key data types:
- `RunOptions`: runtime and artifact behavior
- `App`: target command + args + optional env overrides
- `Key` / `Modifier`: input model
- `Selector`: viewport query model
- `WaitOptions`: polling behavior
- `SnapshotName`: typed snapshot id
- `Tui`: runtime handle passed to DSL actions

Default `RunOptions`:
- `timeoutSeconds = 5`
- `retries = 0`
- `stepRetries = 0`
- `terminalCols = 134`
- `terminalRows = 40`
- `artifactsDir = "artifacts"`
- `ambiguityMode = FailOnAmbiguous`
- `updateSnapshots = False`
- `snapshotTheme = "auto"`

## 4. DSL Language (Shallow Haskell DSL)

### 4.1 Test DSL style

Tests are plain Haskell code, usually with `tasty`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty (defaultMain, testGroup)
import TuiSpec

main :: IO ()
main =
  defaultMain $ testGroup "suite"
    [ tuiTest defaultRunOptions "counter" $ \tui -> do
        launch tui (app "my-tui" [])
        waitForText tui (Exact "Ready")
        press tui (CharKey '+')
        expectSnapshot tui "counter-updated"
    ]
```

### 4.2 Session DSL (non-`tasty`)

Use `withTuiSession` for interactive scripts/tools:

```haskell
withTuiSession defaultRunOptions "demo" $ \tui -> do
  launch tui (app "sh" [])
  sendLine tui "echo hello"
  _ <- dumpView tui "hello"
  pure ()
```

### 4.3 Actions

- `launch :: Tui -> App -> IO ()`
- `app :: FilePath -> [String] -> App`
- `press :: Tui -> Key -> IO ()`
- `pressCombo :: Tui -> [Modifier] -> Key -> IO ()`
- `typeText :: Tui -> Text -> IO ()`
- `sendLine :: Tui -> Text -> IO ()`

`launch` replaces any currently running app for that `Tui` handle.
When `App.env` is provided, launch inherits parent environment variables and:
- `Just "value"` sets/overrides a variable
- `Nothing` unsets an inherited variable

When `App.cwd` is provided, launch runs in that working directory.

### 4.4 Waits and assertions

- `waitFor :: Tui -> WaitOptions -> (Viewport -> Bool) -> IO ()`
- `waitForText :: Tui -> Selector -> IO ()`
- `waitForSelector :: Tui -> WaitOptions -> Selector -> IO ()`
- `expectVisible :: Tui -> Selector -> IO ()`
- `expectNotVisible :: Tui -> Selector -> IO ()`

Behavior:
- polling-based
- default poll interval: `100ms`
- `waitForText` / `expectVisible` use `timeoutSeconds` from `RunOptions`
- matching runs on rendered viewport text (ANSI control/style escapes are interpreted)
- ambiguity checking runs after positive selector match

### 4.5 Selector language

`Selector` constructors:
- `Exact Text`
- `Regex Text`
- `At Int Int`
- `Within Rect Selector`
- `Nth Int Selector`

`Regex` semantics are intentionally lightweight, not full PCRE:
- supports alternation via `|`
- supports wildcard segmenting via `.*`
- strips literal `(` and `)` during matching

Ambiguity mode:
- `FailOnAmbiguous`: fail if selector has multiple matches (except explicit `At`/`Nth`)
- `FirstVisibleMatch`: tolerate multiple matches
- `LastVisibleMatch`: tolerate multiple matches

### 4.6 Input key model

`Key` supports:
- named keys: `Enter`, `Esc`, `Tab`, `Backspace`
- arrows: `ArrowUp`, `ArrowDown`, `ArrowLeft`, `ArrowRight`
- function keys: `FunctionKey 1..12`
- char-oriented keys: `CharKey c`, `Ctrl c`, `AltKey c`, `NamedKey text`

`pressCombo` supports current mappings:
- `[Control] + CharKey c`
- `[Alt] + CharKey c`
- `[Shift] + CharKey c`

### 4.7 Snapshot DSL

- `expectSnapshot :: Tui -> SnapshotName -> IO ()`
- `dumpView :: Tui -> SnapshotName -> IO FilePath`

`expectSnapshot`:
- captures current ANSI buffer into test artifacts
- compares against baseline in `artifacts/snapshots/<test-slug>/`
- creates baseline when missing or when `updateSnapshots = True`

`dumpView`:
- captures only run artifact (no baseline compare)
- used for exploratory workflows and server orchestration

Both produce:
- `<name>.ansi.txt`
- `<name>.meta.json` (`rows`, `cols`)

## 5. Retry and Timeout Semantics

Test-level retry:
- `retries` in `RunOptions` means `retries + 1` attempts
- each attempt gets clean test artifact directory state

Step-level retry:
- `step :: StepOptions -> String -> IO a -> IO a`
- `stepMaxRetries` and `stepRetryDelayMs`

Hard test timeout:
- each test body is wrapped with `timeoutSeconds`

## 6. Environment Overrides

`RunOptions` can be overridden by env vars:
- `TUISPEC_TIMEOUT_SECONDS`
- `TUISPEC_RETRIES`
- `TUISPEC_STEP_RETRIES`
- `TUISPEC_TERMINAL_COLS`
- `TUISPEC_TERMINAL_ROWS`
- `TUISPEC_ARTIFACTS_DIR`
- `TUISPEC_UPDATE_SNAPSHOTS`
- `TUISPEC_AMBIGUITY_MODE` (`fail|first|first-visible`)
- `TUISPEC_AMBIGUITY_MODE` (`fail|first|first-visible|last|last-visible`)
- `TUISPEC_SNAPSHOT_THEME`

Root/path helpers:
- `TUISPEC_PROJECT_ROOT` overrides project root detection

Theme auto-resolution:
- when theme is `auto`, background is inferred from `COLORFGBG` when available
- fallback is dark (`pty-default-dark`)

## 7. Artifact Layout

For test slug `my-test` under `artifactsDir`:

- test attempt snapshots:
  - `artifacts/tests/my-test/snapshots/<snapshot>.ansi.txt`
  - `artifacts/tests/my-test/snapshots/<snapshot>.meta.json`

- baseline snapshots:
  - `artifacts/snapshots/my-test/<snapshot>.ansi.txt`
  - `artifacts/snapshots/my-test/<snapshot>.meta.json`

For ad-hoc sessions (`withTuiSession "session-name"`):
- `artifacts/sessions/session-name/snapshots/<snapshot>.ansi.txt`
- `artifacts/sessions/session-name/snapshots/<snapshot>.meta.json`

Console summary includes snapshot artifact paths on test completion.

## 8. Rendering

CLI command: `tuispec render`
- input: ANSI snapshot (`.ansi.txt`)
- output: PNG
- metadata (`rows`, `cols`) auto-loaded from adjacent `.meta.json`
- optional overrides: `--rows`, `--cols`, `--theme`, `--font`

CLI command: `tuispec render-text`
- input: ANSI snapshot
- output: visible viewport text
- same metadata and optional size overrides

PNG renderer implementation details:
- uses `python3` + Pillow
- resolves font in this order:
  - `--font`
  - `TUISPEC_FONT_PATH`
  - built-in system fallback font paths

CLI command: `tuispec replay`
- input: JSONL recording file
- options: `--speed as-fast-as-possible|real-time`
- if recording contains `frame` events, replays them visually on the terminal
  (clear screen + render each frame with original timing)
- falls back to printing raw request lines when no frames are present

### 8.1 Recording format

Recording JSONL files contain one event per line:
- `timestampMicros`: microseconds since POSIX epoch
- `direction`: `request|response|notification|frame|frame-delta`
- `line`: raw JSON-RPC line, viewport text, or delta payload

Frame events are captured by a background sampling thread during
`recording.start` at a configurable rate (default 200ms = 5 Hz).
Consecutive identical frames are deduplicated. Full keyframes (`frame`)
are emitted roughly every second; compact line-level deltas
(`frame-delta`) are emitted in between. Delta payloads are JSON arrays
of `[lineIndex, "text"]` pairs.

## 9. JSON-RPC Server

CLI command:

```bash
tuispec server --artifact-dir PATH [--cols N] [--rows N] [--timeout-seconds N] [--ambiguity-mode fail|first-visible|last-visible]
```

Transport:
- newline-delimited JSON-RPC 2.0 on stdin/stdout

Session model:
- one active session at a time
- initialize with `initialize`
- all non-initialize methods require an active session

Methods:
- `initialize`
- `launch`
- `sendKey`
- `sendText`
- `sendLine`
- `currentView`
- `dumpView`
- `renderView`
- `waitUntil`
- `diffView`
- `expectSnapshot`
- `waitForText`
- `expectVisible`
- `expectNotVisible`
- `viewSubscribe`
- `viewUnsubscribe`
- `batch`
- `recording.start` (optional `frameIntervalMs`, default 200 = 5 Hz)
- `recording.stop`
- `recording.status`
- `replay`
- `server.ping`
- `server.shutdown`

Server error codes:
- JSON-RPC standard: parse / invalid request / method not found / invalid params
- server domain:
  - `-32001` no active session
  - `-32002` session already started
  - `-32004` method failed

### 9.1 `launch` params

`launch` accepts:
- `command` (string)
- `args` (array of strings, optional)
- `env` (object of string-to-(string|null) pairs, optional)
- `cwd` (string, optional)
- `readySelector` (selector, optional)
- `readyTimeoutMs` (int, optional)
- `readyPollIntervalMs` (int, optional)

Example:

```json
{"method":"launch","params":{"command":"sh","args":[],"env":{"APP_MODE":"test","CLAUDECODE":null},"cwd":"."}}
```

`env` string values override inherited process env variables for that launch.
`env` null values unset inherited variables for that launch.

### 9.2 `sendKey` format

Accepted string forms:
- base: `Enter`, `Esc`, `Tab`, `Backspace`, arrows, `F1..F12`, single char
- combos: `Ctrl+X`, `Alt+X`, `Shift+X`

### 9.3 Selector JSON format

```json
{"type":"exact","text":"Ready"}
{"type":"regex","pattern":"Ready|Done"}
{"type":"at","col":10,"row":2}
{"type":"within","rect":{"col":1,"row":1,"width":40,"height":10},"selector":{"type":"exact","text":"Ready"}}
{"type":"nth","index":1,"selector":{"type":"exact","text":"Task"}}
```

Coordinate notes:
- server row/col coordinates are 1-based
- `currentView` row/col filters support `0` wildcard for entire row/column ranges

### 9.4 Shutdown semantics

`server.shutdown`:
- sends `SIGKILL` to active child process group
- exits server immediately
- does not wait for graceful teardown

`SIGHUP` handling:
- same behavior as hard shutdown (`SIGKILL` + immediate exit)

## 10. Test Runner Contract

- `tuiTest` is the canonical integration point for `tasty`
- tests are black-box: interaction only via PTY I/O and visible viewport state
- tests are isolated by fresh PTY launch lifecycle per test
- framework remains generic to any TUI binary runnable from shell

## 11. Related docs

- `README.md`: usage overview and quick-start
- `SKILL.md`: REPL and server operator workflow
- `SERVER.md`: server protocol reference
