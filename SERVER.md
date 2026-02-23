# `tuispec server` Plan

This document captures the agreed plan for implementing a JSONRPC server in `tuispec`.

## Goals

- Add `tuispec server` for interactive TUI orchestration over stdin/stdout.
- Speak JSON-RPC 2.0 using the Hackage package `jsonrpc` (module `JSONRPC`).
- Support one active PTY-backed session per server process.
- Expose methods matching the DSL surface so agents can drive TUIs interactively.
- Use `--artifact-dir` as the base output path for dumped views and metadata.

## Transport

- Framing: newline-delimited JSON objects (one JSON-RPC message per line).
- Input: stdin.
- Output: stdout.
- Mode: request/response for v1 (no server notifications).

## Dependency

- Add dependency: `jsonrpc` (package `jsonrpc`, module `JSONRPC`).
- Use protocol types/constants from `JSONRPC`:
  - `JSONRPCRequest`
  - `JSONRPCResponse`
  - `JSONRPCError`
  - `JSONRPCErrorInfo`
  - `RequestId`
  - `rPC_VERSION`
  - `pARSE_ERROR`
  - `iNVALID_REQUEST`
  - `mETHOD_NOT_FOUND`
  - `iNVALID_PARAMS`
  - `iNTERNAL_ERROR`

## CLI

Add command:

```bash
tuispec server --artifact-dir PATH [--cols N] [--rows N] [--timeout-seconds N] [--ambiguity-mode fail|first-visible]
```

Defaults:

- `cols = 134`
- `rows = 40`
- `timeout-seconds = 5`
- `ambiguity-mode = fail`

## Session Model

- Single active session at a time.
- Explicit session initialization method:
  - `initialize`
- Any non-lifecycle method requires an active session.

## Method Surface

### Lifecycle

- `initialize`
  - params:
    - `name` (optional, default `"session"`)
    - optional run overrides:
      - `timeoutSeconds`
      - `terminalCols`
      - `terminalRows`
      - `ambiguityMode`
      - `snapshotTheme`
      - `updateSnapshots`
  - result:
    - `sessionName`
    - `artifactRoot`
    - `rows`
    - `cols`

### Orchestration

- `launch`
  - params:
    - `command`
    - `args`

- `sendKey`
  - params:
    - `key` (string)
  - accepted forms:
    - `Enter`, `Esc`, `Tab`, `Backspace`
    - `ArrowUp`, `ArrowDown`, `ArrowLeft`, `ArrowRight`
    - `F1`..`F12`
    - `Ctrl+X`, `Alt+X`
    - single char like `"a"`

- `sendText`
  - params:
    - `text`

- `sendLine`
  - params:
    - `text`

- `currentView`
  - result:
    - `text`
    - `rows`
    - `cols`

- `dumpView`
  - params:
    - `name`
  - result:
    - `snapshotPath`
    - `metaPath`

### Assertions / waits

- `expectVisible`
  - params:
    - `selector`

- `expectNotVisible`
  - params:
    - `selector`

- `waitForText`
  - params:
    - `selector`
    - optional `timeoutMs`
    - optional `pollIntervalMs`

- `expectSnapshot`
  - params:
    - `name`
  - result:
    - `ok`
    - plus snapshot paths where available

### Server utility

- `server.ping`
  - result:
    - `pong: true`
    - `version`

- `server.shutdown`
  - result:
    - `shuttingDown: true`
  - then exit process (no explicit session teardown)

## Selector Encoding

Use tagged object format:

- exact:
  - `{"type":"exact","text":"Ready"}`
- regex:
  - `{"type":"regex","pattern":"Ready|Done"}`
- at:
  - `{"type":"at","col":10,"row":2}`
- within:
  - `{"type":"within","rect":{"col":0,"row":0,"width":40,"height":10},"selector":{...}}`
- nth:
  - `{"type":"nth","index":1,"selector":{...}}`

## Error Contract

JSON-RPC errors:

- parse failures: `pARSE_ERROR`
- invalid request: `iNVALID_REQUEST`
- unknown method: `mETHOD_NOT_FOUND`
- bad params: `iNVALID_PARAMS`
- internal failures: `iNTERNAL_ERROR`

Server-domain error codes:

- `-32001` `NoActiveSession`
- `-32002` `SessionAlreadyStarted`
- `-32004` `MethodFailed`

Error `data` should include:

- `kind`
- `details`
- optional `hint`

## Artifact Layout

With `--artifact-dir X` and session `foo`:

- `X/sessions/foo/snapshots/<name>.ansi.txt`
- `X/sessions/foo/snapshots/<name>.meta.json`

`dumpView` writes only run artifacts.  
`expectSnapshot` keeps existing DSL baseline semantics unless changed later.

## Implementation Units

1. Add module `src/TuiSpec/Server.hs`:
   - JSONRPC request decode
   - method dispatch
   - session state (`IORef (Maybe ActiveSession)`)
2. Update `app/Main.hs`:
   - add `server` subcommand and options
3. Update `tuispec.cabal`:
   - add `jsonrpc` dependency
4. Update `SKILL.md`:
   - add JSONRPC server usage examples and workflow

## Validation

- Build/tests:
  - `cabal build`
  - `cabal test`
  - `(cd example && cabal test)`
- Manual server smoke:
  - start `tuispec server`
  - send `initialize`, `launch`, `sendKey`, `dumpView`, `server.shutdown`
  - verify artifact files and response payloads
