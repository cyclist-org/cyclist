#!/bin/bash

logdir=$1
iterations=$2

if [ -z "$iterations" ]; then
  iterations=1
fi

now=$(date +"%Y-%m-%d_%H-%M-%S")
sha=""
if [ -d ".git" ]; then
  sha="$(git rev-parse --short HEAD)_"
fi

pwd

opts=("--inf-desc vla" "--inf-desc legacy" "--inf-desc sla" "--inf-desc fwk-full --no-minimality" "--inf-desc fwk-full" "--inf-desc fwk-or --no-minimality --ord 0" "--inf-desc fwk-or --no-minimality --ord 1" "--inf-desc fwk-or --no-minimality --ord 2" "--inf-desc fwk-or --ord 0" "--inf-desc fwk-or --ord 1" "--inf-desc fwk-or --ord 2")
opts=("--inf-desc fwk-or")


for i in ${!opts[@]}; do
  opt=${opts[i]}
  suffix=$(echo "$opt" | sed -r 's/\s+//g')
  for j in $(seq 1 1 $iterations); do
    (set -x; TST_OPTS="$opt -s -p" make fo-tests) > "$logdir/fo-tests_${sha}$now$suffix.$j.log" 2>&1
    (set -x; TST_OPTS="$opt -s -p" make sl-tests) > "$logdir/sl-tests_${sha}$now$suffix.$j.log" 2>&1
  done
done