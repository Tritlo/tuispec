# Changelog

## 0.3.1.0 — 2026-06-06

* Add mouse click simulation. DSL: `click`/`clickWith` (0-based coordinate) and `clickSelector`/`clickSelectorWith` (Playwright-style, resolves a selector to its match origin and honors `ambiguityMode`). New `MouseButton`, `MouseEncoding`, and `ClickOptions`/`defaultClickOptions` types. JSON-RPC: a `click` method accepting either `{col,row}` (1-based) or `{selector}`, with optional `button` and `encoding`. Clicks emit a press + release; SGR encoding is the default (what `vty`/Brick enable), with an `x10` override.

## 0.3.0.0 — 2026-04-30

* **Breaking:** the `App` type is now a sum (`App` | `HaskellApp`) so it no longer derives `Eq`. The `Show` instance is kept (now hand-written). `command`, `args`, `appName`, and `appAction` accessors are partial — pattern match on the constructor when both shapes are possible.

* Add `haskellApp` for launching a Haskell `IO ()` action under a PTY without a separate target executable.
* Expose `artifactRoot` to let tests locate the per-test directory for logs and auxiliary files.
* Add Haskell DSL helpers for ready-gated launches (`launchAndWait`), artifact paths/writes, env-file loading, PATH prepending, numbered choice selection, manual failure bundles, and JSONL recording export.
* Add `TuiSpec.Choice` for parsing and selecting numbered choices without tying the API to a modal or menu widget.
* Add `currentViewRect` for retrieving viewport text cropped to a `Rect`.
* Locate the built `tuispec` executable in both `dist-newstyle` and `dist` build trees, and honor a `TUISPEC_EXECUTABLE` override, so the test suite works under `Setup.hs`/`v1-build` (#1).
* Surface "launched app exited" failures from `press`, `pressCombo`, and `typeText` instead of writing into a closed PTY.
* Parse numbered choice labels correctly when the line contains a trailing border (e.g. `> 1) Haskell │ Modern` now yields `Haskell`, not `Haskell Modern`).
* Switch internal logs (actions, frames, warnings, snapshot artifacts) to amortised-O(1) prepend storage; the `actionLog`/`frameLog`/`snapshotLog`/`runtimeWarnings` fields of `TuiState` are now newest-first (reverse before display).
* Add `recordTraceTo :: Maybe FilePath` to `RunOptions`. When set, the runner writes a JSONL trace recording with real wall-clock timestamps to that path under each test's artifact directory; replay with `tuispec replay PATH`. **Breaking:** `frameLog` is now `[(Int64, Text)]` (microseconds-since-session-start, frame text), not `[Text]`, and `Tui` gained a `tuiSessionStart :: UTCTime` field.
* Automatically write a diagnostic failure bundle for failed Haskell DSL tests.
* Surface the underlying exception when PTY launch fails instead of reporting a generic "backend unavailable" error.
* Fail waits when the launched app exits before the selector or stability condition is reached.
* Skip the post-test viewport sync when no PTY is active, avoiding spurious work after a failed launch.

## 0.2.0.0 — 2026-02-24

* Add `waitForStable` primitive (DSL + JSON-RPC) to replace fixed sleeps with debounce-based viewport stability checks.
* Overhaul `SKILL.md` around a golden-path workflow using `waitForStable`, with a wait-strategy decision guide and common failure troubleshooting.
* Introduce server v2 APIs including launch `cwd`/nullable `env`, per-call ambiguity control, filtered `currentView`, render-in-one-step snapshot flows, notifications, batch operations, and JSONL recording/replay.
* Add `tuispec replay` CLI command and expand end-to-end protocol coverage in tests and docs.
* Record viewport frames in JSONL sessions and replay them on the terminal with `tuispec replay`, using delta compression for efficient frame storage. Keyframe and delta metadata are preserved through replay for frame reconstruction.
* Add interactive replay controls: Space to pause/resume, Left/Right to step frames, Up/Down to skip 5 frames, r to redraw from last keyframe, q to quit.
* Add `--show-input` replay flag to display the last input action on a status line below the viewport.
* Add `--version` flag to the CLI.
* Include tuispec version in `initialize` response and snapshot `meta.json`.
* Fix theme-aware color rendering for light/dark PNG snapshots.
* Use alternate screen buffer for replay to avoid polluting terminal scrollback.
* Fix test flakiness caused by orphaned PTY child processes between server tests.

## 0.1.1.1 — 2026-02-23

* Fix PTY teardown to avoid hangs when waiting for child exit by using bounded `getProcessExitCode` polling.
* Add regression coverage and documentation clarifying that selector matching runs on rendered viewport text (ANSI control/style escapes are interpreted).

## 0.1.1.0 — 2026-02-23

* Add optional `env` parameter to `launch` (DSL + JSON-RPC) to override process environment variables.
* Add `app` constructor helper and update docs/examples to use it.

## 0.1.0.0 — 2026-02-23

* Initial release
