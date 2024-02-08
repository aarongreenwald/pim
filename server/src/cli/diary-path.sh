#! /bin/bash

##################
## Usage: from the root directory of the notes repo, `diary-path.sh 20240208 1`

## The script outputs to stdout the path of the diary entry for the given date,
## if the second argument is '1' it also creates the file if it doesn't exist.
## If the second argument is not '1', the script returns an error if the file doesn't exist.

## TODO: Support for multiple diaries or the primary diary in a non-default location.
## 1. Check if ./diary exists and is the primary diary. If not, look for the primary diary.
## 2. If no primary diary exists, create one in ./diary, unless the directory already exists, in which case throw.
##################

date=$1
createIfNotExists=$2
relativePath=./diary/$date

if [ -e "$relativePath" ]; then
    echo -n $relativePath
elif [ "$createIfNotExists" == "1" ]; then
    touch $relativePath
    echo -n $relativePath
else
    exit 1 #file doesn't exist and createIfNotExists is false
fi

