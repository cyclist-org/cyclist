#!/bin/bash

logdir=$1

opts=("-spot" "-SD" "-XSD" "-rel-ext" "-rel-ext -min" "-rel-ext -scc" "-rel-ext -ff" "-rel-ext -min -scc" "-rel-ext -min -ff" "-rel-ext -min -scc -ff" "-rel-ext -full" "-rel-ext -full -min" "-rel-ext -full -scc" "-rel-ext -full -ff" "-rel-ext -full -min -scc" "-rel-ext -full -min -ff" "-rel-ext -full -min -scc -ff" )

now=$(date +"%Y-%m-%d_%H-%M-%S")

for i in ${!opts[@]}; do
  opt=${opts[i]}
  suffix=$(echo "$opt" | sed -r 's/\s+//g')
  (set -x; TST_OPTS="$opt -s -p" make sl-tests) > "$logdir/sl-tests_$now$suffix.log" 2>&1
done