#!/usr/bin/env bash
set -eu -o pipefail
set -x

unset PGUSER
unset PGDATABASE

if [[ -z "$PGDATA" ]]; then
    echo "PGDATA must be set!" >&2
    exit 1
fi

initdb_args=(
    --encoding=utf-8
    --locale=en_US.UTF-8
)

if [[ ! -d "$PGDATA" ]]; then
    initdb "${initdb_args[@]}"
fi

exec postgres -k "$PGHOST" \
    -c listen_addresses=
