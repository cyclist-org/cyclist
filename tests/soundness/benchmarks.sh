#!/bin/bash

logdir=$1

now=$(date +"%Y-%m-%d_%H-%M-%S")

spot_suffix="${now}_spot.log"
rel_suffix="${now}_rel.log"

opts=("-spot" "-rel-ext -full -ff" "-rel-ext -ff" "-rel-ext -min -ff" "-rel-ext -scc -ff" "-rel-ext -min -scc -ff")

opts_with_sd=("${opts[@]}")
opts_with_sd+=("-SD")

# Experiments run on Ubuntu 20.04.3, 16GB RAM
# Experiements run with Spot 2.10.4


# test1.exe - positive examples, n varies, p = 1

# -rel-ext -min -scc -ff n = 450 ~60s
# -rel-ext -min -scc -ff n = 457 OUT OF MEMORY

inputs="1 $(seq 25 25 450)"
for n in ${inputs[@]}; do
  for i in ${!opts_with_sd[@]}; do
    opt=${opts_with_sd[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    (set -x; time dune exec tests/soundness/test1.exe -- $opt -rel-stats $n) >> "$logdir/test1_benchmarks_${now}$suffix.log" 2>&1
  done
done


# test2.exe - positive examples, n = 1, p varies

# -rel-ext -min -scc -ff n = 200 ~40s
# -rel-ext -min -scc -ff n = 250 ~100s
# -rel-ext -min -scc -ff n = 300 ~200s
# -rel-ext -min -scc -ff n = 350 ~365s
# -rel-ext -min -scc -ff n = 375 ~475s
# -rel-ext -min -scc -ff n = 379 OUT OF MEMORY

inputs="1 $(seq 20 20 180)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts_with_sd[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    (set -x; time dune exec --context=test2-benchmarks tests/soundness/test2.exe -- $opt -rel-stats $n) >> "$logdir/test2_benchmarks_${now}$suffix.log" 2>&1
  done
done


# test3.exe - negative example

inputs="1 $(seq 250 250 20000)"
for n in ${inputs[@]}; do
  for i in ${!opts[@]}; do
    opt=${opts[i]}
    suffix=$(echo "$opt" | sed -r 's/\s+//g')
    (set -x; time dune exec tests/soundness/test3.exe -- $opt -rel-stats $n) >> "$logdir/test3_benchmarks_${now}$suffix.log" 2>&1
  done
done
