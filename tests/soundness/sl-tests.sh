#!/bin/bash

logdir=$1
iterations=$2

if [ -z "$iterations" ]; then
  iterations=1
fi

opts=("-SD" "-rel-ext" "-rel-ext -min" "-rel-ext -scc" "-rel-ext -ff" "-rel-ext -min -scc" "-rel-ext -min -ff" "-rel-ext -min -scc -ff" "-rel-ext -full" "-rel-ext -full -min" "-rel-ext -full -scc" "-rel-ext -full -ff" "-rel-ext -full -min -scc" "-rel-ext -full -min -ff" "-rel-ext -full -min -scc -ff" )

now=$(date +"%Y-%m-%d_%H-%M-%S")

for i in ${!opts[@]}; do
  opt=${opts[i]}
  suffix=$(echo "$opt" | sed -r 's/\s+//g')
  for j in $(seq 1 1 $iterations); do
    (set -x; TST_OPTS="$opt -s -p" make sl-tests) > "$logdir/sl-tests_$now$suffix.$j.log" 2>&1
  done
done