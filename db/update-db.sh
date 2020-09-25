#!/bin/bash

# First argument is the path to the database file, eg ~/data
./backup-db.sh $1
# TODO: if error return
cat ./views.sqlite.sql | sqlite3 $1/pim.db
# TODO run migration scripts based on input db version