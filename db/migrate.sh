#!/usr/bin/env bash

DB_DIR="$(dirname $0)"

refinery migrate -e POSTGRES_CONNECTION_STRING -p "$DB_DIR/migrations"
