#!/bin/sh
set -e

# Resolve the tests/ directory relative to this script, so the script works
# both inside Docker (/tests/) and in CI (repo checkout).
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TESTS_DIR="${TESTS_DIR:-$(cd "$SCRIPT_DIR/../tests" && pwd)}"

# Point XDG (and thus the auth DB and data dir) at a clean temp directory
# so the server and the admin commands all share the same state.
export HOME=$(mktemp -d)
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
mkdir -p "$XDG_DATA_HOME/benchpress" "$XDG_CONFIG_HOME"

PORT="${PORT:-8083}"
CONFIG="$HOME/config.sexp"

# Config with two provers and a benchmark directory.
# "always-true" completes instantly; "slow" sleeps 30 s per file for cancel tests.
cat > "$CONFIG" << 'EOF'
(import-prelude false)

(prover
  (name always-true)
  (cmd "/bin/true $file"))

(prover
  (name slow)
  (cmd "sleep 30; true $file"))

(dir
  (path "/tmp/bench")
  (expect (const unknown)))
EOF

# Create test benchmark files
mkdir -p /tmp/bench
touch /tmp/bench/a.fake /tmp/bench/b.fake

# Start server in its own process group so its shutdown signals don't
# propagate back to this script.
setsid benchpress-server serve \
    --port "$PORT" \
    --config "$CONFIG" \
    &
SERVER_PID=$!

cleanup() {
    kill "$SERVER_PID" 2>/dev/null || true
    rm -rf "$HOME" /tmp/bench
}
trap cleanup EXIT

# Wait for the server to accept connections (up to 4 seconds)
for i in $(seq 1 20); do
    if curl -sf "http://localhost:$PORT/" >/dev/null 2>&1; then
        break
    fi
    sleep 0.2
done

# user1: key1 for general tests, key3 to be revoked later
USER1_ID=$(benchpress-server user create \
    --email user1@benchpress.local \
    | awk '{print $NF}')
API_KEY1=$(benchpress-server api-key create \
    --user "$USER1_ID" \
    | awk '{print $NF}')
API_KEY3=$(benchpress-server api-key create \
    --user "$USER1_ID" \
    | awk '{print $NF}')

# user2: key2 for cross-user isolation tests
USER2_ID=$(benchpress-server user create \
    --email user2@benchpress.local \
    | awk '{print $NF}')
API_KEY2=$(benchpress-server api-key create \
    --user "$USER2_ID" \
    | awk '{print $NF}')

echo "Server pid=$SERVER_PID port=$PORT user1=$USER1_ID user2=$USER2_ID"

# Auth tests: public routes + unauthenticated rejection
hurl \
    --variable "host=localhost:$PORT" \
    --variable "api_key=$API_KEY1" \
    "$TESTS_DIR"/api_auth.hurl \
    --test

# Lifecycle tests: happy path, cross-user isolation, cancellation
hurl \
    --variable "host=localhost:$PORT" \
    --variable "api_key1=$API_KEY1" \
    --variable "api_key2=$API_KEY2" \
    "$TESTS_DIR"/api_lifecycle.hurl \
    --test

# Result-file population and idempotency
hurl \
    --variable "host=localhost:$PORT" \
    --variable "api_key=$API_KEY1" \
    "$TESTS_DIR"/api_result_file.hurl \
    --test

# benchpressctl smoke tests
BENCHPRESS_SERVER="localhost:$PORT" BENCHPRESS_API_KEY="$API_KEY1" \
    sh "$TESTS_DIR"/ctl_smoke.sh

# Revoke key3, then verify it is rejected while key1 still works
benchpress-server api-key revoke --key "$API_KEY3"
hurl \
    --variable "host=localhost:$PORT" \
    --variable "revoked_key=$API_KEY3" \
    --variable "valid_key=$API_KEY1" \
    "$TESTS_DIR"/api_revoke.hurl \
    --test

echo "==> All tests passed!"
