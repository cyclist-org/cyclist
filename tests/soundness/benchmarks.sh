#!/bin/bash

logdir=$1
iterations=$2

if [ -z "$iterations" ]; then
  iterations=1
fi

now=$(date +"%Y-%m-%d_%H-%M-%S")
sha=$(git rev-parse --short HEAD)

base_opts=("-VLA" "-SLA" "-OP -min -scc -ff")


# test1.exe - positive examples, n varies, p = 1

opts=("${base_opts[@]}")

inputs="1 $(seq 25 25 600)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test1.exe -- $opt -rel-stats $n) >> "$logdir/test1_benchmarks_${sha}_${now}$suffix.$j.log" 2>&1
    done
  done
done


# test2.exe - positive examples, n = 1, p varies

opts=("${base_opts[@]}")

inputs="1 $(seq 25 25 600)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec --context=test2-benchmarks tests/soundness/test2.exe -- $opt -rel-stats $n) >> "$logdir/test2_benchmarks_${sha}_${now}$suffix.$j.log" 2>&1
    done
  done
done


# test3.exe - negative example

opts=("${base_opts[@]}")

inputs="1 $(seq 250 250 4000)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test3.exe -- $opt -rel-stats $n) >> "$logdir/test3_benchmarks_${sha}_${now}$suffix.$j.log" 2>&1
    done
  done
done
