#!/usr/bin/env bash
set -eu -o pipefail
set -x
# Sets up the database user and creates the dev database

unset PGUSER
unset PGDATABASE
pg_user=slacklinker
pg_database=slacklinker-development

psql --dbname postgres <<EOF
DO \$\$
  BEGIN
      CREATE ROLE "$pg_user" WITH LOGIN SUPERUSER;
      EXCEPTION WHEN duplicate_object THEN RAISE NOTICE '%, skipping', SQLERRM USING ERRCODE = SQLSTATE;
  END
\$\$;
EOF

psql --dbname postgres <<EOF
SELECT 'CREATE DATABASE "$pg_database" WITH OWNER = "$pg_user"' WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = '$pg_database')\gexec
EOF

psql --dbname postgres <<EOF
GRANT ALL PRIVILEGES ON DATABASE "$pg_database" TO "$pg_user";
\c "$pg_database"
GRANT ALL PRIVILEGES ON SCHEMA public TO "$pg_user";
EOF

