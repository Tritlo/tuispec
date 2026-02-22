# tuispec v1 Specification

Date: 2026-02-22  
Status: Draft for implementation

## 1. Purpose

`tuispec` is an internal Haskell testing framework for terminal UIs (TUIs), optimized for agent-driven testing.  
It is inspired by Playwright ergonomics, but targets Linux terminal apps running inside `pty`.

Primary v1 goal:
- Let developers and agents write reliable black-box TUI tests with reproducible artifacts.

## 2. Scope

### 2.1 In Scope (v1)

- Linux only.
- One `pty` pane per test.
- Fresh `pty` session per test (isolation per test).
- Haskell test scripts using a shallow DSL.
- Selector-style querying on the visible viewport (exact text, regex text, coordinate/region/nth-match targeting).
- Input actions with key presses and chord combos (`Ctrl`, `Alt`, function keys).
- Assertions with Playwright-like explicit waiting.
- PNG snapshots with in-repo baselines and update flow.
- Asciinema recording and frame extraction support.
- Retry support (test-level and step-level).
- CLI + JSON outputs, plus Markdown report linking artifacts.
- Headless mode with artifacts for local + CI (GitHub Actions).

### 2.2 Out of Scope (v1)

- Multi-pane or multi-window interactions.
- Framework-specific instrumentation hooks.
- Full scrollback assertions (viewport only in v1).
- Windows/macOS support.
- Heavy UI tooling (trace viewer app, web dashboard).

## 3. Non-Goals and Design Principles

### 3.1 Non-Goals

- Not a generic terminal emulator platform.
- Not a visual pixel-perfect golden system for every dynamic UI detail.
- Not a replacement for app-level unit/integration tests.

### 3.2 Principles

- Keep it simple and explicit.
- Favor deterministic artifacts over rich but flaky automation.
- Fail loudly on ambiguous selector matches by default.
- Prefer explicit `waitFor` and assertion polling over implicit magic.
- Keep agent-friendly execution: stable CLI, stable artifact paths, machine-readable JSON.

## 4. Dependencies

Required runtime tools:
- `pty`
- Linux terminal environment

Optional runtime tools:
- `asciinema` (if recording backend is enabled)
- external cast-to-frame renderer (if asciinema-frame PNG backend is selected)

Build/runtime language:
- Haskell (`cabal` only in v1)

## 5. User Experience

### 5.1 Runner Modes

Developers can run tests via:
- `tuitest run path/to/MyTests.hs`
- `cabal test`

Execution mode:
- Headless only in v1 (artifacts-first workflow).

### 5.2 Minimal Test Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import TuiSpec

main :: IO ()
main = runSuite defaultRunOptions
  [ test "stream cancellation" $ \tui -> do
      launch tui $ App
        { command = "colimit-code"
        , args = []
        }

      waitForText tui (Exact "Specify")
      typeText tui "write me a simple counter, and include a machine and cli and http platform to verify it"
      press tui Enter

      waitForText tui (Regex "Specify.*complete|Done")

      press tui (Ctrl 's')
      press tui Enter
      waitForText tui (Regex "language|typescript")
      typeText tui "typescript"
      press tui Enter

      waitForText tui (Regex "verifying")
      press tui Esc

      waitForText tui (Regex "cancelled|stopped|interrupted")
      typeText tui "verify the http platform first"
      press tui Enter

      expectVisible tui (Regex "http platform")
      expectSnapshot tui "stream-cancel-post-followup"
  ]
```

### 5.3 DSL Surface (v1)

Core:
- `test :: String -> (Tui -> IO ()) -> Spec`
- `runSuite :: RunOptions -> [Spec] -> IO ExitCode`
- `launch :: Tui -> App -> IO ()`

Actions:
- `press :: Tui -> Key -> IO ()`
- `pressCombo :: Tui -> [Modifier] -> Key -> IO ()`
- `typeText :: Tui -> Text -> IO ()`

Selectors:
- `Exact Text`
- `Regex Text`
- `At Col Row`
- `Within Rect Selector`
- `Nth Int Selector`

Assertions and waits:
- `expectVisible :: Tui -> Selector -> IO ()`
- `expectNotVisible :: Tui -> Selector -> IO ()`
- `waitFor :: Tui -> WaitOptions -> (Viewport -> Bool) -> IO ()`
- `waitForText :: Tui -> Selector -> IO ()`
- `expectSnapshot :: Tui -> SnapshotName -> IO ()`

Retries:
- test-level retries via `RunOptions`
- step-level retries via `step :: StepOptions -> String -> IO a -> IO a`

## 6. Semantics

### 6.1 Isolation

- Each test gets a unique `pty` server/socket + session + pane.
- No state is shared across tests.
- Tests are executed sequentially in v1.

### 6.2 Launch Contract

Per test app config requires:
- `command :: FilePath`
- `args :: [String]`

Defaults:
- terminal size: `120x40`
- working directory: process cwd
- env: inherited

### 6.3 Wait and Timeout Model

- Default assertion timeout: `5s`.
- Polling interval default: `100ms`.
- Minimal implicit waiting applies only inside assertions/wait functions.
- Actions (`press`, `typeText`) are immediate.

### 6.4 Selector Ambiguity

Default behavior:
- ambiguous selector match fails with clear diagnostics.

Configurable behavior:
- allow “first visible match” via option.

### 6.5 Retry Model

Supported:
- test-level retries (fresh pty session per attempt)
- step-level retries

Step-level retry constraints:
- intended for assertion/wait steps.
- action retries are allowed but marked risky in report metadata.

Artifact retention:
- keep final attempt only (v1 decision).

## 7. Artifact Model

### 7.1 Per-Test Artifact Bundle

On failure, emit:
- PNG snapshot(s)
- raw visible viewport text
- JSON step log
- asciinema recording

On success:
- emit minimal metadata and optional checkpoints as configured.

## 7.2 Snapshot Baselines

- Baselines stored in-repo (`snapshots/`).
- `--update-snapshots` rewrites baselines from current run.
Snapshot compare uses light normalization:
- ignore cursor blink effects.
- trim trailing whitespace noise.
- normalize non-semantic ANSI artifacts where possible.

### 7.3 PNG Sources

Two supported snapshot render backends:
- `pty` capture renderer
- asciinema-frame renderer

One backend is selected as default in `RunOptions`, with ability to override per test or CLI.

### 7.4 Report Outputs

Required:
- CLI summary
- JSON machine report

Additional:
- Markdown report with links to expected/actual/diff PNGs, raw viewport text dumps, asciinema cast, and per-test JSON logs.

Suggested artifact layout:

```text
artifacts/
  run-2026-02-22T15-00-00Z/
    report.md
    report.json
    tests/
      stream-cancellation/
        attempt-final/
          viewport.txt
          screen-actual.png
          screen-expected.png
          screen-diff.png
          session.cast
          steps.json
snapshots/
  stream-cancellation/
    stream-cancel-post-followup.png
```

## 8. Internal Architecture

### 8.1 Components

1. Runner
- loads specs and executes sequentially
- handles retries and lifecycle

2. pty transport
- session creation, pane lifecycle, input injection, viewport capture

3. Screen model
- parses visible pane capture to normalized viewport model
- supports selector queries and coordinate math

4. Assertion engine
- polling, timeout handling, diagnostics

5. Snapshot engine
- baseline I/O
- normalization
- image diff

6. Recorder
- records session cast (asciinema-compatible output)
- provides frame extraction hooks

7. Reporter
- CLI
- JSON
- Markdown with artifact links

### 8.2 pty Lifecycle

Per test:
1. Start isolated pty server/session with configured size.
2. Launch app command in pane.
3. Execute test script actions/assertions.
4. On completion, collect artifacts and teardown session.

### 8.3 Data Contracts

`Viewport`:
- dimensions
- visible text cells
- style/attribute map (best effort in v1)
- timestamp

`StepLog`:
- step id/name/type
- start/end timestamps
- action payload (sanitized)
- selector info
- result + error
- artifact references

`TestResult`:
- status
- duration
- retries
- artifact root

## 9. CLI Specification (v1)

### 9.1 Commands

- `tuitest run <file-or-module>`
- `tuitest list <file-or-module>`

### 9.2 Key Flags

- `--timeout <seconds>`
- `--retries <n>` (test-level)
- `--step-retries <n>`
- `--snapshot-source pty|asciinema`
- `--update-snapshots`
- `--artifacts-dir <path>`
- `--json <path>`
- `--markdown-report <path>`
- `--fail-on-ambiguous` / `--first-match`

### 9.3 Exit Codes

- `0`: all tests passed
- `1`: test failures
- `2`: framework/runtime/configuration error

## 10. Configuration Model

Project-level config file (optional):
- `tuispec.yaml`

Config precedence:
1. CLI flags
2. test script options
3. `tuispec.yaml`
4. framework defaults

Example:

```yaml
timeoutSeconds: 5
retries: 1
stepRetries: 1
terminal:
  cols: 120
  rows: 40
snapshot:
  source: pty
  normalization: light
report:
  json: artifacts/report.json
  markdown: artifacts/report.md
selectors:
  ambiguity: fail
```

## 11. CI Contract (GitHub Actions)

Required CI behavior:
- deterministic headless run
- persisted artifacts on failure
- JSON report export for bot/agent consumption

Suggested CI command:

```bash
cabal test --test-show-details=direct
```

Or:

```bash
tuitest run test/Spec.hs --json artifacts/report.json --markdown-report artifacts/report.md
```

## 12. Security and Privacy

v1 assumptions:
- no special compliance constraints declared.

Still required:
- sanitize command-line secrets in logs where feasible.
- avoid recording environment variables in cleartext unless explicitly requested.

## 13. Example: Stream Cancellation Scenario Mapping

Scenario source: `~/Colimit/colimit-code/test-script.md`.

Must validate:
1. `Esc` stops visible stream quickly.
2. App remains active after cancellation.
3. Follow-up prompt is accepted.
4. Session continues with follow-up instruction.

Suggested explicit checks:
- `waitForText "verifying"` before cancel
- after `Esc`, `expectNotVisible` on known streaming indicator with short timeout window
- assert shell/process still alive in pane (e.g., prompt/input remains active)
- assert follow-up content appears after submission

## 14. Implementation Milestones

### Milestone 1: Core Runner + pty Transport

- Spec registration/execution
- isolated per-test pty lifecycle
- key press + text input
- visible viewport capture
- basic CLI pass/fail

### Milestone 2: Selectors + Assertions + Wait Engine

- exact/regex selectors
- coordinate + nth + region selectors
- ambiguity handling
- 5s polling defaults

### Milestone 3: Snapshots + Diff + Markdown/JSON Reports

- PNG render from pty source
- baseline storage + `--update-snapshots`
- light normalization
- Markdown report with links

### Milestone 4: Asciinema + Retry Enhancements

- cast recording integration
- optional frame extraction backend
- test + step retry support finalized
- final CI hardening

## 15. Acceptance Criteria for v1

- A developer can author and run a Haskell DSL test using `test "name" $ \tui -> ...`.
- Tests run headlessly in Linux with fresh pty per test.
- Stream cancellation scenario passes with stable artifacts.
- Failures produce PNG + viewport text + JSON log + cast file.
- Snapshot updates and comparisons work via `--update-snapshots`.
- JSON + Markdown reports are generated and CI-friendly.

## 16. Open Items (Need Final Decision Before Build Freeze)

- Exact step-level retry API shape (`step` combinator options and defaults).
- Default snapshot backend (`pty` vs `asciinema`) for first release.
- Canonical stable text markers for the Colimit scenario assertions.
