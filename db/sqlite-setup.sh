#!/bin/bash

# First argument is the path to the database file, eg ~/data/pim.db
rm $1
cat ./schema.sqlite3.sql | sqlite3 $1