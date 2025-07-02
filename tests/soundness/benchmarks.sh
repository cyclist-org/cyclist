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


# test1.exe - positive examples, n varies, p = 1

opts=("--inf-desc vla" "--inf-desc legacy" "--inf-desc sla" "--inf-desc fwk-or --no-minimality" "--inf-desc fwk-or --no-minimality --ord 1" "--inf-desc fwk-or" "--inf-desc fwk-or --ord 1")

inputs="1 $(seq 25 25 600)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test1.exe -- $opt $n) >> "$logdir/test1_benchmarks_${sha}${now}$suffix.$j.log" 2>&1
    done
  done
done

opts=("--inf-desc vla" "--inf-desc legacy" "--inf-desc sla" "--inf-desc fwk-or --no-minimality --ord 1" "--inf-desc fwk-or --ord 1")

inputs="$(seq 700 100 2000)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test1.exe -- $opt $n) >> "$logdir/test1_benchmarks_${sha}${now}$suffix.$j.log" 2>&1
    done
  done
done

opts=("--inf-desc fwk-full --no-minimality" "--inf-desc fwk-full")

inputs="1 $(seq 25 25 300)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test1.exe -- $opt $n) >> "$logdir/test1_benchmarks_${sha}${now}$suffix.$j.log" 2>&1
    done
  done
done

# test2.exe - positive examples, n = 1, p varies

opts=("--inf-desc vla" "--inf-desc legacy" "--inf-desc sla" "--inf-desc fwk-full --no-minimality" "--inf-desc fwk-full" "--inf-desc fwk-or --no-minimality" "--inf-desc fwk-or")

inputs="1 $(seq 25 25 600)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec --context=test2-benchmarks tests/soundness/test2.exe -- $opt $n) >> "$logdir/test2_benchmarks_${sha}${now}$suffix.$j.log" 2>&1
    done
  done
done


# test3.exe - negative examples, n varies, p = 1

opts=("--inf-desc vla" "--inf-desc legacy" "--inf-desc sla" "--inf-desc fwk-full --no-minimality" "--inf-desc fwk-full" "--inf-desc fwk-or --no-minimality" "--inf-desc fwk-or")

inputs="1 $(seq 250 250 4000)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test3.exe -- $opt $n) >> "$logdir/test3_benchmarks_${sha}${now}$suffix.$j.log" 2>&1
    done
  done
done

opts=("--inf-desc fwk-or --no-minimality --ord 1" "--inf-desc fwk-or --ord 1")

inputs="1 $(seq 250 250 2000)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test3.exe -- $opt $n) >> "$logdir/test3_benchmarks_${sha}${now}$suffix.$j.log" 2>&1
    done
  done
done
