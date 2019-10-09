#!/bin/sh
set -ex

if ! [ -e ./tables.sql ]; then
  echo "Couldn't find tables.sql, check your directory"
  exit 1
fi

PORT=5999
DBNAME=requery-example

# Create the database, if it's not there
if ! echo | psql -p "$PORT" "$DBNAME" >/dev/null; then
  createdb -p "$PORT" "$DBNAME"
fi

# Create tables in the database
psql -p "$PORT" "$DBNAME" < ./tables.sql
