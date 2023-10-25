#!/bin/bash

logdir=$1
iterations=$2

if [ -z "$iterations" ]; then
  iterations=1
fi

now=$(date +"%Y-%m-%d_%H-%M-%S")
sha=$(git rev-parse --short HEAD)

opts=("-VLA" "-legacy" "-SLA" "-FWK -scc -ff" "-FWK -min -scc -ff" "-OR -scc -ff -ord 0" "-OR -scc -ff -ord 1" "-OR -scc -ff -ord 2" "-OR -min -scc -ff -ord 0" "-OR -min -scc -ff -ord 1" "-OR -min -scc -ff -ord 2")

for i in ${!opts[@]}; do
  opt=${opts[i]}
  suffix=$(echo "$opt" | sed -r 's/\s+//g')
  for j in $(seq 1 1 $iterations); do
    (set -x; TST_OPTS="$opt -s -p" make fo-tests) > "$logdir/fo-tests_${sha}_$now$suffix.$j.log" 2>&1
    (set -x; TST_OPTS="$opt -s -p" make sl-tests) > "$logdir/sl-tests_${sha}_$now$suffix.$j.log" 2>&1
  done
done