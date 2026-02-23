# Changelog

## 0.1.1.1 — 2026-02-23

* Fix PTY teardown to avoid hangs when waiting for child exit by using bounded `getProcessExitCode` polling.
* Add regression coverage and documentation clarifying that selector matching runs on rendered viewport text (ANSI control/style escapes are interpreted).

## 0.1.1.0 — 2026-02-23

* Add optional `env` parameter to `launch` (DSL + JSON-RPC) to override process environment variables.
* Add `app` constructor helper and update docs/examples to use it.

## 0.1.0.0 — 2026-02-23

* Initial release
