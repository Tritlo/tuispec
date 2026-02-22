# tuispec

`tuispec` is a starter Haskell framework for black-box TUI testing with Playwright-like ergonomics.

Current starter status:
- shallow DSL for tests
- sequential runner with retries
- artifact folder scaffolding
- `tuitest` CLI command shape (`run`, `list`)

## Quick start

Build:

```bash
cabal build
```

Run the smoke test:

```bash
cabal test
```

Run a test script directly:

```bash
cabal run tuitest -- run test/Spec.hs
```

List tests from a script:

```bash
cabal run tuitest -- list test/Spec.hs
```

## Layout

- `src/TuiSpec.hs`: public API
- `src/TuiSpec/Types.hs`: DSL and configuration types
- `src/TuiSpec/Runner.hs`: starter runner implementation
- `app/Main.hs`: `tuitest` CLI entrypoint
- `test/Spec.hs`: smoke test example
- `tuispec-v1-spec.md`: project specification
