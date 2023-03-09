#!/bin/bash

logdir=$1
iterations=$2

if [ -z "$iterations" ]; then
  iterations=1
fi

opts=("-VLA" "-SLA" "-OP -min -scc -ff" )
# opts=("-spot-new")
now=$(date +"%Y-%m-%d_%H-%M-%S")
sha=$(git rev-parse --short HEAD)

for i in ${!opts[@]}; do
  opt=${opts[i]}
  suffix=$(echo "$opt" | sed -r 's/\s+//g')
  for j in $(seq 1 1 $iterations); do
    (set -x; TST_OPTS="$opt -s -p" make sl-tests) > "$logdir/sl-tests_${sha}_$now$suffix.$j.log" 2>&1
    (set -x; TST_OPTS="$opt -s -p" make fo-tests) > "$logdir/fo-tests_${sha}_$now$suffix.$j.log" 2>&1
  done
done