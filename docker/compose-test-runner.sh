#!/bin/sh
# Test runner for docker compose test setup.
# The server is already running (separate container, health-checked).
# We share the XDG volumes so admin commands can access the same auth DB.
set -e

SERVER_HOST="${SERVER_HOST:-server}"
PORT="${PORT:-8083}"
HOST="$SERVER_HOST:$PORT"

# Create bench files if not already present
mkdir -p /tmp/bench
touch /tmp/bench/a.fake /tmp/bench/b.fake

echo "==> Creating test users and API keys (server: $HOST)"

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

echo "user1=$USER1_ID user2=$USER2_ID"

echo "==> Running auth tests"
hurl \
    --variable "host=$HOST" \
    --variable "api_key=$API_KEY1" \
    /tests/api_auth.hurl \
    --test

echo "==> Running lifecycle tests"
hurl \
    --variable "host=$HOST" \
    --variable "api_key1=$API_KEY1" \
    --variable "api_key2=$API_KEY2" \
    /tests/api_lifecycle.hurl \
    --test

echo "==> Running result-file tests"
hurl \
    --variable "host=$HOST" \
    --variable "api_key=$API_KEY1" \
    /tests/api_result_file.hurl \
    --test

echo "==> Running benchpressctl tests"
BENCHPRESS_SERVER="$HOST" BENCHPRESS_API_KEY="$API_KEY1" \
    sh /tests/ctl_smoke.sh

echo "==> Running revoke tests"
benchpress-server api-key revoke --key "$API_KEY3"
hurl \
    --variable "host=$HOST" \
    --variable "revoked_key=$API_KEY3" \
    --variable "valid_key=$API_KEY1" \
    /tests/api_revoke.hurl \
    --test

echo "==> All tests passed!"
