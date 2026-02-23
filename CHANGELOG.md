# Changelog

## 0.2.0.0 — 2026-02-24

* Introduce server v2 APIs including launch `cwd`/nullable `env`, per-call ambiguity control, filtered `currentView`, render-in-one-step snapshot flows, notifications, batch operations, and JSONL recording/replay.
* Add `tuispec replay` CLI command and expand end-to-end protocol coverage in tests and docs.
* Record viewport frames in JSONL sessions and replay them on the terminal with `tuispec replay`, using delta compression for efficient frame storage.
* Add interactive replay controls: Space to pause/resume, Left/Right to step frames, Up/Down to skip 5 frames, q to quit.
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
