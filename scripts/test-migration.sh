#!/usr/bin/env bash
set -eu

# This script is a helper for quickly iterating migrations based on
# snapshotting the database with templates.

scriptdir="$(dirname "$0")"
arg="${1:-}"

PG_ARGS="-U slacklinker -h localhost -p 5432"
PG_ARGS_CREATEDROP="$PG_ARGS --maintenance-db=postgres"
DEV_DB=slacklinker

confirm() {
    echo "$1"
    echo -n "[Y/n] "
    read confirmation
    case "$confirmation" in
        y | Y | yes | "")
            return 0 ;;
        *)
            return 1 ;;
    esac
}

ensure_clone_exists() {
    # adapted from https://stackoverflow.com/questions/24806122/get-database-creation-date-on-postgresql
    sql="SELECT now() - (pg_stat_file('base/'||oid ||'/PG_VERSION')).modification > interval '1 day' FROM pg_database where datname = '$DEV_DB-clone';"
    db_older_than_1d="$(psql $PG_ARGS -d postgres -A -t -z -c "$sql")"
    if [[ -z "$db_older_than_1d" ]]; then
        echo -e "Clone does not exist! First, run \n    db/snapshot.sh save" >&2
        exit 1
    elif [[ "$db_older_than_1d" == "t" ]]; then
        echo 'You requested to clone a database older than one day over your development database' >&2
        if ! confirm 'Do you intend to do this?'; then
            echo 'bailing :)' >&2
            exit 1
        fi
    elif [[ "$db_older_than_1d" == "f" ]]; then
        echo 'DB is from the last day, should be good' >&2
    else
        echo 'psql returned something unexpected, aborting' >&2
        echo "$db_older_than_1d"
        exit 1
    fi
}

do_restore() {
    say ensure_clone_exists
    say dropdb $PG_ARGS_CREATEDROP "$DEV_DB"
    say createdb $PG_ARGS_CREATEDROP -T "$DEV_DB-clone" "$DEV_DB"
}

say() {
    echo ">>> $@" >&2
    "$@"
}

print_usage() {
    cat >&2 <<EOF
Usage: $0 COMMAND

Test your migrations and reset your dev database in less than 5s.

Commands:
save
    Saves the state of the development database at a point in time. This
    command creates a database "$DEV_DB-clone", using the
    development database as a template. This saves both data and schema.

restore
    Restores the development database to the snapshotted state:
    drops the development database and copies "$DEV_DB-clone"
    into its place.

migrate
    Restores the development database before running migrations. Equivalent to
    "$0 restore && db/migrate.sh".

    This lets you iterate a migration by saving prior to applying it, then
    repeatedly running "$0 migrate"
EOF
    exit 1
}

case "$arg" in
save)
    say dropdb $PG_ARGS_CREATEDROP --if-exists "$DEV_DB-clone"
    say createdb $PG_ARGS_CREATEDROP -T "$DEV_DB" "$DEV_DB-clone"
    ;;

restore)
    do_restore
    ;;

migrate)
    do_restore
    say db/migrate.sh
    ;;

*)
    print_usage
    ;;
esac
