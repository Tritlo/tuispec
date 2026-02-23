#!/bin/bash
# Record a demo session of the example Brick TUI app via tuispec server.
# Produces a JSONL recording at artifacts/demo-recording/session.jsonl.
#
# Usage:
#   ./test/record-demo.sh [delay_seconds]
#
# Replay:
#   cabal run tuispec -- replay artifacts/demo-recording/session.jsonl --show-input
set -e

cd "$(dirname "$0")/.."

DELAY="${1:-1}"
RECORDING_PATH="artifacts/demo-recording/session.jsonl"

# Build everything first
echo "Building tuispec and tui-demo..."
cabal build tuispec 2>&1 | tail -1
(cd example && cabal build tui-demo 2>&1 | tail -1)

TUI_DEMO="$(cd example && cabal list-bin tui-demo 2>/dev/null)"

rm -f "$RECORDING_PATH"

send_and_sleep() {
  echo "$1"
  sleep "$2"
}

echo "Recording demo (delay=${DELAY}s between actions)..."

(
  send_and_sleep '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"name":"demo-recording","timeoutSeconds":30}}' 0.2
  send_and_sleep '{"jsonrpc":"2.0","id":2,"method":"launch","params":{"command":"'"$TUI_DEMO"'","args":[],"readySelector":{"type":"exact","text":"Agent Readiness"}}}' 0.2
  send_and_sleep '{"jsonrpc":"2.0","id":3,"method":"recording.start","params":{"path":"'"$RECORDING_PATH"'","frameIntervalMs":200}}' "$DELAY"
  # Navigate to board view
  send_and_sleep '{"jsonrpc":"2.0","id":4,"method":"sendKey","params":{"key":"b"}}' "$DELAY"
  # Move down and toggle items
  send_and_sleep '{"jsonrpc":"2.0","id":5,"method":"sendKey","params":{"key":"j"}}' "$DELAY"
  send_and_sleep '{"jsonrpc":"2.0","id":6,"method":"sendText","params":{"text":" "}}' "$DELAY"
  send_and_sleep '{"jsonrpc":"2.0","id":7,"method":"sendKey","params":{"key":"j"}}' "$DELAY"
  send_and_sleep '{"jsonrpc":"2.0","id":8,"method":"sendText","params":{"text":" "}}' "$DELAY"
  # Move back up
  send_and_sleep '{"jsonrpc":"2.0","id":9,"method":"sendKey","params":{"key":"k"}}' "$DELAY"
  send_and_sleep '{"jsonrpc":"2.0","id":10,"method":"sendKey","params":{"key":"k"}}' "$DELAY"
  # Switch to logs view
  send_and_sleep '{"jsonrpc":"2.0","id":11,"method":"sendKey","params":{"key":"l"}}' "$DELAY"
  # Quit
  send_and_sleep '{"jsonrpc":"2.0","id":12,"method":"sendKey","params":{"key":"q"}}' "$DELAY"
  send_and_sleep '{"jsonrpc":"2.0","id":13,"method":"recording.stop","params":{}}' 0.2
  echo '{"jsonrpc":"2.0","id":14,"method":"server.shutdown","params":null}'
) | cabal run tuispec -- server --artifact-dir artifacts/demo-recording

echo ""
echo "Recording saved to: $RECORDING_PATH"
echo "Replay with: cabal run tuispec -- replay $RECORDING_PATH --show-input"
