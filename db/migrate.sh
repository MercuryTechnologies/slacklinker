#!/usr/bin/env bash
set -eu

DB_DIR="$(dirname $0)"

# Refinery doesn't support Unix socket paths in connection URIs, so we bridge
# with socat to a temporary TCP port.
# https://github.com/rust-db/refinery/issues/258

# Pick a random ephemeral port
SOCAT_PORT=$((49152 + RANDOM % 16384))

socat "TCP-LISTEN:${SOCAT_PORT},fork,reuseaddr" "UNIX-CONNECT:$PGHOST/.s.PGSQL.5432" &
SOCAT_PID=$!
cleanup() { kill "$SOCAT_PID" 2>/dev/null || true; wait "$SOCAT_PID" 2>/dev/null || true; }
trap cleanup EXIT

# Wait for socat to be listening
while ! nc -z localhost "$SOCAT_PORT" 2>/dev/null; do :; done

POSTGRES_CONNECTION_STRING="postgres://slacklinker@localhost:${SOCAT_PORT}/slacklinker-development" \
    refinery migrate -e POSTGRES_CONNECTION_STRING -p "$DB_DIR/migrations"
