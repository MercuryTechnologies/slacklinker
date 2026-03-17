#!/usr/bin/env bash
set -eu

DB_DIR="$(dirname "$0")"

PGPORT="${PGPORT:-5432}"

# Extract the host from the connection string
SOCK_HOST=$(python3 -c "
import os
from urllib.parse import urlparse, parse_qs
u = urlparse(os.environ['POSTGRES_CONNECTION_STRING'])
qs = parse_qs(u.query)
print(qs.get('host', [''])[0])
")

if [[ "${SOCK_HOST}" == /* ]]; then
    # Refinery doesn't support Unix socket paths in connection URIs, so we bridge
    # with socat to a temporary TCP port.
    # https://github.com/rust-db/refinery/issues/258

    # Pick a random ephemeral port
    SOCAT_PORT=$((49152 + RANDOM % 16384))

    socat "TCP-LISTEN:${SOCAT_PORT},fork,reuseaddr" "UNIX-CONNECT:${SOCK_HOST}/.s.PGSQL.$PGPORT" &
    SOCAT_PID=$!
    cleanup() { kill "$SOCAT_PID" 2>/dev/null || true; wait "$SOCAT_PID" 2>/dev/null || true; }
    trap cleanup EXIT

    # Wait for socat to be listening
    while ! nc -z localhost "$SOCAT_PORT" 2>/dev/null; do :; done

    # Rewrite the connection string to go through socat
    export POSTGRES_CONNECTION_STRING
    POSTGRES_CONNECTION_STRING=$(SOCAT_PORT="$SOCAT_PORT" python3 -c "
import os
from urllib.parse import urlparse, urlencode, parse_qs, urlunparse
u = urlparse(os.environ['POSTGRES_CONNECTION_STRING'])
port = os.environ['SOCAT_PORT']
qs = parse_qs(u.query)
qs.pop('host', None)
query = urlencode(qs, doseq=True)
import getpass
user = u.username or getpass.getuser()
netloc = '{}@localhost:{}'.format(user, port)
print(urlunparse((u.scheme, netloc, u.path, u.params, query, u.fragment)))
")
fi

refinery migrate -e POSTGRES_CONNECTION_STRING -p "$DB_DIR/migrations"
