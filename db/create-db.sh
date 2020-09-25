#!/bin/bash

# creates a new empty database. don't use this unless you plan
# on re-importing all data

# First argument is the path to the database file, eg ~/data
mv $1/pim.db $1/pim_$(date +%s).db
# TODO: if error return
cat ./tables.sqlite.sql | sqlite3 $1/pim.db
cat ./views.sqlite.sql | sqlite3 $1/pim.db