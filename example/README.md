# Brick Demo TUI

This folder contains a multi-pane Brick app used to validate `tuispec`
against richer terminal UX patterns.

Build and run:

```bash
cd tests/tui
CABAL_DIR=$PWD/.cabal cabal build
CABAL_DIR=$PWD/.cabal cabal run tui-demo
```

Run via `tuispec` from repo root:

```bash
CABAL_DIR=$PWD/.cabal cabal run tuitest -- run test/BrickDemoSpec.hs
```

Notes:
- This demo depends on Hackage packages (`brick`, `vty`).
- In fully offline or DNS-restricted environments the build will fail while fetching dependencies.
- The demo emits deterministic `STATE ...` lines for test selectors and snapshots.
- Keybindings include pane focus cycling (`tab`), split toggles (`s`), screen tabs (`g/b/l`), board navigation (`j/k/space`), command mode (`/`, `enter`, `esc`), and global controls (`q`, `?`, `t`).
