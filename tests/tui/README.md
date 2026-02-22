# Brick Demo TUI

This folder contains an expanded Brick-based TUI app used to validate
`tuispec` against a real interactive terminal UI flow.

Build and run:

```bash
cd tests/tui
CABAL_DIR=$PWD/.cabal cabal build
CABAL_DIR=$PWD/.cabal cabal run tui-demo
```

Run via `tuispec` from repo root:

```bash
CABAL_DIR=$PWD/.cabal cabal run tuitest -- run test/BrickDemoSpec.hs --timeout 35 --retries 1 --step-retries 1 --artifacts-dir artifacts/brick-showcase --update-snapshots
```

Notes:
- This demo depends on Hackage packages (`brick`, `vty`).
- In fully offline or DNS-restricted environments the build will fail while fetching dependencies.
- The demo emits deterministic `STATE ...` lines for test selectors and snapshots.
