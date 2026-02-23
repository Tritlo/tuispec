# AGENTS

## Project Info
- Name: `tuispec`
- Language/tooling: Haskell + `cabal` only
- Scope: Playwright-like black-box testing for TUIs using PTY transport
- Platform target: Linux terminal environments
- Snapshot mode: text + PNG artifacts
- Primary specification: `SPEC.md`

## Defaults To Keep
- Terminal viewport default: `134` columns x `40` rows
- Styled snapshot renderer: `12pt` font, `1px` padding on each side
- Snapshot palette mode: `auto` (fallback to dark when terminal background is unknown)

## Required Workflow
When changing Haskell or cabal files:
1. Run formatter:
   - `fourmolu -i $(rg --files -g '*.hs')`
2. Normalize cabal file:
   - `cabal-gild -i tuispec.cabal`
3. Build:
   - `cabal build`

## Useful Test Commands
- Smoke test:
  - `cabal test`
- Brick demo suite:
  - `cd example && cabal test`
