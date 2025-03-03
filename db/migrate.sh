#!/usr/bin/env bash
set -eu

DB_DIR="$(dirname $0)"

refinery migrate -e POSTGRES_CONNECTION_STRING -p "$DB_DIR/migrations"
