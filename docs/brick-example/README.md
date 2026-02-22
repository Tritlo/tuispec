# Brick Example Artifacts

These files are committed outputs from running:

```bash
CABAL_DIR=$PWD/.cabal cabal run tuitest -- run test/BrickDemoSpec.hs --timeout 35 --retries 1 --step-retries 1 --artifacts-dir artifacts/brick-showcase --update-snapshots
```

Included artifacts:

- `01-initial.png`
- `02-counter-updated.png`
- `03-checklist-progress.png`
- `04-theme-help.png`
- `05-reset-isolation.png`
- `report.md`
- `report.json`
