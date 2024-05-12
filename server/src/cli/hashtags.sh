#!/bin/bash

## Expectes argument: be directory to grep
## Eg: ~/code/pim/server/src/cli/hashtags.sh ~/code/notes   

## a hashtag is any # preceeded by a space or beginning of line and followed by a lowercase character,
## Multiple can be on the same line and will be counted separately
grep -Ehr "(^| )#[a-z]" --exclude-dir=".git" $1 | tr " " "\n" | grep "^#[a-z]" | sort | uniq -c | sort -nr

## View single eg: grep -Er "(^| )#somehashtag" $1

