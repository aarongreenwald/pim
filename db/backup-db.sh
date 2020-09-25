#!/bin/bash

# First argument is the path to the database file, eg ~/data
cp $1/pim.db $1/pim_$(date +%s).db
