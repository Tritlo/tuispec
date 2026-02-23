# Brick Demo TUI

This folder contains a multi-pane Brick app used to validate `tuispec`
against richer terminal UX patterns.

Build and run the demo app:

```bash
cd example
CABAL_DIR=$PWD/.cabal cabal build
CABAL_DIR=$PWD/.cabal cabal run tui-demo
```

Run the integration suite:

```bash
cd example
cabal test
```

Notes:
- This demo depends on Hackage packages (`brick`, `vty`).
- In fully offline or DNS-restricted environments the build will fail while fetching dependencies.
- Keybindings include pane focus cycling (`tab`), split toggles (`s`), screen tabs (`g/b/l`), board navigation (`j/k/space`), and global controls (`q`, `?`).
