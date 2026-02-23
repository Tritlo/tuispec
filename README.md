# tuispec

`tuispec` is a Haskell library for Playwright-like black-box testing of terminal UIs over PTY.

Current design:
- tests are regular compiled Haskell programs (`tasty` + `tuispec`)
- PTY transport only
- per-test isolation (fresh PTY process per test)
- text selectors + keypress/input actions
- snapshot assertions with text + PNG artifacts
- optional REPL-like session mode for ad-hoc exploration

## Quick start

Build the library:

```bash
cabal build
```

Run root smoke tests:

```bash
cabal test
```

Run the Brick demo integration suite:

```bash
cd example
cabal test
```

## Snapshot layout

For a test named `my test` (slug: `my-test`) and `artifactsDir = "artifacts"`:

- Baselines:
  - `artifacts/snapshots/my-test/<snapshot>.ansi.txt`
  - `artifacts/snapshots/my-test/<snapshot>.meta.json`
- Per-run capture:
  - `artifacts/tests/my-test/snapshots/<snapshot>.ansi.txt`
  - `artifacts/tests/my-test/snapshots/<snapshot>.meta.json`

Render any ANSI snapshot to PNG:

```bash
cabal run tuispec -- render artifacts/tests/my-test/snapshots/<snapshot>.ansi.txt
```

`render` reads rows/cols from `<snapshot>.meta.json` automatically, with optional overrides via `--rows`, `--cols`, and `--theme` (theme defaults to `auto`).

Render visible plain text from the ANSI stream (terminal-emulated):

```bash
cabal run tuispec -- render-text artifacts/tests/my-test/snapshots/<snapshot>.ansi.txt
```

`render-text` reads rows/cols from `<snapshot>.meta.json` automatically, with optional `--rows` and `--cols` overrides.

## REPL-style usage

For ad-hoc sessions (outside `tasty`), use `withTuiSession` and `dumpView`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import TuiSpec

main :: IO ()
main =
  withTuiSession
    defaultRunOptions { artifactsDir = "artifacts/repl" }
    "demo-repl"
    $ \tui -> do
        launch tui (App "my-tui" [])
        sendLine tui "/help"
        _ <- dumpView tui "step-1"
        sendLine tui "q"
```

Artifacts land under:

- `artifacts/repl/sessions/demo-repl/snapshots/<name>.ansi.txt`
- `artifacts/repl/sessions/demo-repl/snapshots/<name>.meta.json`

## Minimal example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty (defaultMain, testGroup)
import TuiSpec

main :: IO ()
main =
  defaultMain $ testGroup "demo"
    [ tuiTest defaultRunOptions "counter" $ \tui -> do
        launch tui (App "my-tui" [])
        waitForText tui (Exact "Ready")
        press tui (CharKey '+')
        expectSnapshot tui "counter-updated"
        press tui (CharKey 'q')
    ]
```

## Layout

- `src/TuiSpec.hs`: public API
- `src/TuiSpec/Types.hs`: DSL and configuration types
- `src/TuiSpec/Runner.hs`: PTY runner + assertions + snapshot renderer
- `test/Spec.hs`: root smoke test
- `example/app/Main.hs`: Brick demo app
- `example/test/IntegrationSpec.hs`: integration test suite using `tuispec`

## Brick example

- [Brick integration spec](example/test/IntegrationSpec.hs)
- [Brick app](example/app/Main.hs)

## Notes

- PNG rendering requires `python3` with Pillow (`PIL`).
- Default viewport is `134x40`.
- Snapshot theme mode defaults to `auto` (falls back to dark if unknown).
