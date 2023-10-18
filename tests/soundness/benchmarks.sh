#!/bin/bash

logdir=$1
iterations=$2

if [ -z "$iterations" ]; then
  iterations=1
fi

now=$(date +"%Y-%m-%d_%H-%M-%S")
sha=$(git rev-parse --short HEAD)

base_opts=("-VLA" "-SLA" "-OR -min -scc -ff")


# test1.exe - positive examples, n varies, p = 1

opts=("-VLA" "-legacy" "-SLA" "-OR -scc -ff" "-OR -scc -ff -ord 1" "-OR -min -scc -ff" "-OR -min -scc -ff -ord 1")

inputs="1 $(seq 25 25 600)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test1.exe -- $opt $n) >> "$logdir/test1_benchmarks_${sha}_${now}$suffix.$j.log" 2>&1
    done
  done
done

opts=("-FWK -scc -ff" "-FWK -min -scc -ff")

inputs="1 $(seq 25 25 300)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test1.exe -- $opt $n) >> "$logdir/test1_benchmarks_${sha}_${now}$suffix.$j.log" 2>&1
    done
  done
done

opts=("-VLA" "-legacy" "-SLA" "-OR -scc -ff -ord 1" "-OR -min -scc -ff -ord 1")

inputs="$(seq 700 100 2000)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test1.exe -- $opt $n) >> "$logdir/test1_benchmarks_${sha}_${now}$suffix.$j.log" 2>&1
    done
  done
done

# test2.exe - positive examples, n = 1, p varies

opts=("-VLA" "-legacy" "-SLA" "-FWK -scc -ff" "-FWK -min -scc -ff" "-OR -scc -ff" "-OR -min -scc -ff")

inputs="1 $(seq 25 25 600)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec --context=test2-benchmarks tests/soundness/test2.exe -- $opt $n) >> "$logdir/test2_benchmarks_${sha}_${now}$suffix.$j.log" 2>&1
    done
  done
done


# test3.exe - negative examples, n varies, p = 1

opts=("-VLA" "-legacy" "-SLA" "-FWK -scc -ff" "-FWK -min -scc -ff" "-OR -scc -ff" "-OR -min -scc -ff")

inputs="1 $(seq 250 250 4000)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test3.exe -- $opt $n) >> "$logdir/test3_benchmarks_${sha}_${now}$suffix.$j.log" 2>&1
    done
  done
done

opts=("-OR -scc -ff -ord 1" "-OR -min -scc -ff -ord 1")

inputs="1 $(seq 250 250 2000)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    for j in $(seq 1 1 $iterations); do
      (set -x; time dune exec tests/soundness/test3.exe -- $opt $n) >> "$logdir/test3_benchmarks_${sha}_${now}$suffix.$j.log" 2>&1
    done
  done
done
