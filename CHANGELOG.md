# Changelog

## Unreleased

* Fix `TUISPEC_ARTIFACTS_DIR` handling by creating custom artifact directories before canonicalization
* Fix server `waitForText` option merging to preserve session-level timeout defaults when only partial overrides are provided
* Refactor duplicated project-root discovery logic into shared `TuiSpec.ProjectRoot`

## 0.1.0.0 — 2026-02-23

* Initial release
* PTY-based test runner with per-test isolation
* Shallow Haskell DSL: `launch`, `press`, `pressCombo`, `typeText`, `sendLine`
* Selector language: `Exact`, `Regex`, `At`, `Within`, `Nth`
* Wait/assertion primitives: `waitForText`, `expectVisible`, `expectNotVisible`
* Snapshot support: `expectSnapshot` (baseline comparison), `dumpView` (capture only)
* ANSI text + PNG snapshot artifacts
* `tasty` integration via `tuiTest`
* REPL-style sessions via `withTuiSession`
* JSON-RPC server for interactive orchestration (`tuispec server`)
* CLI commands: `render` (ANSI to PNG), `render-text` (ANSI to plain text)
* Configurable timeouts, retries, terminal size, ambiguity modes
* Environment variable overrides for all `RunOptions` fields
