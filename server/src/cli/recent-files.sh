#! /bin/bash

# This script sums the number of changes (additions + deletions) per file in recent commits
# and returns the results in descending order
# It overweights small commits by adding 5 lines for each individual commit, meaning that 2 one-line commits
# counts for more than a single commit changing 6 lines.

# First argument is number of commits to include in calculation
# second argument is number of files to return

# TODO remove deleted files, figure out how to track renames correctly
# (both in terms of follow and the way the data is outputted)
# Consider alternate ways to count frequency. Is it possible to count bytes changed?
# Perhaps the lookback needs to be changed, or somehow consider date?

#Note: awk separator must be \t so that filenames with spaces work
git log --numstat --format= -n "$1" |
  awk -F '\t' '
    { a[$3] += ($1 + $2 + 5); }
    END {
      for (i in a) {
        printf "%-15s\t%s\n", i, a[i];
      }
    }
  ' |
  sort -rnk2 |
  head -$2 |
  awk -F '\t' '{print $1}'