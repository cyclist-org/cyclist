#!/bin/bash

logdir=$1
iterations=$2

if [ -z "$iterations" ]; then
  iterations=1
fi

now=$(date +"%Y-%m-%d_%H-%M-%S")
sha=$(git rev-parse --short HEAD)

base_opts=("-spot" "-rel-ext -min -scc -ff")

xtd_opts=("${base_opts[@]}")
xtd_opts+=("-rel-ext -full -ff" "-rel-ext -ff" "-rel-ext -min -ff" "-rel-ext -scc -ff")

# Experiments run on Ubuntu 20.04.3, 16GB RAM
# Experiements run with Spot 2.10.4


# test1.exe - positive examples, n varies, p = 1

# -rel-ext -min -scc -ff n = 450 ~60s
# -rel-ext -min -scc -ff n = 457 OUT OF MEMORY (before 9e40c42)

opts=("${base_opts[@]}")
# SD and XSD run out of memory at about 16 <= n <= 26
# opts+=("-SD" "-XSD")

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

# -rel-ext -min -scc -ff n = 200 ~40s
# -rel-ext -min -scc -ff n = 250 ~100s
# -rel-ext -min -scc -ff n = 300 ~200s
# -rel-ext -min -scc -ff n = 350 ~365s
# -rel-ext -min -scc -ff n = 375 ~475s
# -rel-ext -min -scc -ff n = 379 OUT OF MEMORY (before 9e40c42)

opts=("${base_opts[@]}")
# XSD displays exponential behaviour (1s for p = 8, 15s for p = 9, 374s for p = 10)
# opts+=("-XSD")

inputs="1 $(seq 25 25 300)"
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

# -rel-ext -min -scc -ff n = 21639 OUT OF MEMORY (before 9e40c42)

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
