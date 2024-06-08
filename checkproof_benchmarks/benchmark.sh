cd ../
out_dir="./checkproof_benchmarks/logs"
for method in "SH" "OR" "FWK"; do
    for i in {1..5}; do
        dune exec src/generic/checkproof.exe -- -"$method" -min -scc -ff -s --json < ../cyclist_graphs/unminimized/all_graphs.json  > "$out_dir/results.minimization.$method.$i"
    done
done
for method in "SH" "OR" "FWK"; do
    for i in {1..5}; do
        dune exec src/generic/checkproof.exe -- -"$method" -min -scc -ff -s --json --unminimized-proofs < ../cyclist_graphs/unminimized/all_graphs.json  > "$out_dir/results.no_minimization.$method.$i"
    done
done

for method in "VLA" "SLA"; do
    for i in {1..2}; do
        dune exec src/generic/checkproof.exe -- -"$method" -s --json < ../cyclist_graphs/unminimized/all_graphs.json  > "$out_dir/results.minimization."$method".$i"
    done
done
for method in "VLA" "SLA"; do
    for i in {1..2}; do
        dune exec src/generic/checkproof.exe -- -"$method" -s --json --unminimized-proofs < ../cyclist_graphs/unminimized/all_graphs.json  > "$out_dir/results.no_minimization."$method".$i"
    done
done