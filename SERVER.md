# `tuispec server` Reference

This document describes the implemented `tuispec server` JSON-RPC behavior.

## Goals

- Run interactive PTY-backed TUI sessions over JSON-RPC 2.0.
- Keep one active session per server process.
- Provide launch/input/wait/snapshot APIs plus utility methods for batching,
  notifications, and JSONL recording/replay.

## Transport

- Framing: newline-delimited JSON objects (one JSON-RPC message per line).
- Input: stdin.
- Output: stdout.
- Mode: request/response plus optional server notifications.

## CLI

```bash
tuispec server --artifact-dir PATH [--cols N] [--rows N] [--timeout-seconds N] [--ambiguity-mode fail|first-visible|last-visible]
```

Defaults:

- `cols = 134`
- `rows = 40`
- `timeout-seconds = 5`
- `ambiguity-mode = fail`

## Session Model

- Single active session at a time.
- Initialize with `initialize`.
- Non-lifecycle methods require an active session unless noted.

## Coordinate Rules

- Row/column coordinates in server APIs are 1-based.
- `0` is reserved as a wildcard for row/column range selectors in `currentView`
  filters (`entireRow`, `entireCol`, and `region`/`rows` wildcard forms).
- Selector `at`/`within.rect` coordinates must be `>= 1`.

## Method Surface

### Lifecycle

- `initialize`
  - params:
    - `name` (optional, default `"session"`)
    - optional run overrides:
      - `timeoutSeconds`
      - `terminalCols`
      - `terminalRows`
      - `ambiguityMode` (`fail|first|first-visible|last|last-visible`)
      - `snapshotTheme`
      - `updateSnapshots`
  - result:
    - `sessionName`
    - `artifactRoot`
    - `rows`
    - `cols`

- `launch`
  - params:
    - `command` (required)
    - `args` (optional, default `[]`)
    - `env` (optional object of `string -> string|null`)
      - string value: set/override
      - `null`: unset inherited env var
    - `cwd` (optional)
    - `readySelector` (optional selector)
    - `readyTimeoutMs` (optional)
    - `readyPollIntervalMs` (optional)
  - result:
    - `ok`

### Input / Wait

- `sendKey`
  - params: `key`

- `sendText`
  - params: `text`

- `sendLine`
  - params:
    - `text`
    - optional `expectAfter` selector
    - optional `timeoutMs`
    - optional `pollIntervalMs`
    - optional `ambiguityMode`
  - result: `ok`

- `waitForText`
  - params:
    - `selector`
    - optional `timeoutMs`
    - optional `pollIntervalMs`
    - optional `ambiguityMode`
  - result: `ok`

- `waitUntil`
  - params:
    - `pattern` (regex-like pattern over full `currentView.text`)
    - optional `timeoutMs`
    - optional `pollIntervalMs`
  - result: `ok`

### View / Snapshot

- `currentView`
  - optional params (choose exactly one filter mode):
    - `rows`: `{ "start": Int, "end": Int }`
    - `region`: `{ "col": Int, "row": Int, "width": Int, "height": Int }`
    - `entireRow`: `Int`
    - `entireCol`: `Int`
  - result:
    - `text`
    - `rows`
    - `cols`

- `dumpView`
  - params:
    - `name`
    - optional `format`: `ansi|png|both` (default `ansi`)
    - optional render overrides for PNG mode:
      - `theme`
      - `font`
      - `rows`
      - `cols`
  - result:
    - `snapshotPath` (canonical)
    - `metaPath` (canonical)
    - `artifactRoot` (canonical)
    - optional `pngPath`

- `renderView`
  - params:
    - `name`
    - optional render overrides: `theme`, `font`, `rows`, `cols`
  - result:
    - `snapshotPath`
    - `metaPath`
    - `pngPath`
    - `artifactRoot`

- `diffView`
  - params:
    - `leftPath`
    - `rightPath`
    - optional `mode`: `text|styled` (default `text`)
  - result:
    - `changed`
    - `changedLines`
    - `summary`

- `expectSnapshot`
  - params: `name`
  - result:
    - `ok`
    - `actualPath`
    - `baselinePath`
    - `baselineExists`

### Assertions

- `expectVisible`
  - params: `selector`
  - result: `ok`

- `expectNotVisible`
  - params: `selector`
  - result: `ok`

### Notifications

- `viewSubscribe`
  - params (optional):
    - `debounceMs` (default `100`)
    - `includeText` (default `false`)
  - result:
    - `ok`
    - `debounceMs`
    - `includeText`

- `viewUnsubscribe`
  - params: none
  - result: `ok`

When subscribed, the server emits JSON-RPC notifications:

```json
{"jsonrpc":"2.0","method":"view.changed","params":{"rows":40,"cols":134}}
```

If `includeText=true`, notification params also include `text`.

### Batch

- `batch`
  - params:
    - `steps`: array of `{ "method": Text, "params": Value }`
  - execution model:
    - sequential
    - non-atomic
    - stop on first failure
  - result:
    - on success: `{ "ok": true, "completed": N, "results": [...] }`
    - on failure: `{ "ok": false, "completed": N, "results": [...], "errorStep": K, "error": {...} }`

### Recording / Replay (JSONL)

- `recording.start`
  - params: `{ "path": FilePath }`
  - result: `{ "ok": true, "path": FilePath }`

- `recording.stop`
  - params: none
  - result: `{ "ok": true }`

- `recording.status`
  - params: none
  - result: `{ "active": Bool, "path": FilePath|null }`

- `replay`
  - params:
    - `path`
    - optional `speed`: `as-fast-as-possible|real-time`
  - result:
    - `ok`
    - `replayedRequests`
    - `path`
    - `speed`

Recording files are JSONL with one event per line and include:

- `timestampMicros`
- `direction` (`request|response|notification`)
- `line` (raw JSON-RPC line)

### Server Utility

- `server.ping`
  - result:
    - `pong: true`
    - `version`

- `server.shutdown`
  - result:
    - `shuttingDown: true`
  - then hard-shutdown:
    - send `SIGKILL` to active child process group
    - exit process immediately

## Selector Encoding

- exact:
  - `{"type":"exact","text":"Ready"}`
- regex:
  - `{"type":"regex","pattern":"Ready|Done"}`
- at (1-based):
  - `{"type":"at","col":10,"row":2}`
- within (1-based origin):
  - `{"type":"within","rect":{"col":1,"row":1,"width":40,"height":10},"selector":{...}}`
- nth:
  - `{"type":"nth","index":1,"selector":{...}}`

## Error Contract

JSON-RPC errors:

- parse failures: `pARSE_ERROR`
- invalid request: `iNVALID_REQUEST`
- unknown method: `mETHOD_NOT_FOUND`
- bad params: `iNVALID_PARAMS`

Server-domain codes:

- `-32001` no active session
- `-32002` session already started
- `-32004` method failed

## Signal Behavior

On `SIGHUP`, server does hard-shutdown:

- send `SIGKILL` to active child process group
- close recording handle
- exit immediately
