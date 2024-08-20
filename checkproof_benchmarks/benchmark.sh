

cd ../
out_dir="./checkproof_benchmarks/logs"

fo_graphs_dir="../cyclist_graphs/cyclist_output_graphs/fo"
sl_graphs_dir="../cyclist_graphs/cyclist_output_graphs/sl"


for i in {1..3}; do
    echo "running $i..."
    find ../cyclist_graphs/cyclist_output_graphs/sl -maxdepth 1 -name "*.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -SH -min -scc -ff -s -R json  > "$out_dir/results.TM.sl.$i" 
    find ../cyclist_graphs/cyclist_output_graphs/fo -maxdepth 1 -name "*.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -SH -min -scc -ff -s -R json  > "$out_dir/results.TM.fo.$i" 
    echo done $i
done


# for method in "OR" "FWK"; do
#     for i in {1..5}; do
#         find ../cyclist_graphs/cyclist_output_graphs/sl -maxdepth 1 -name "*.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -"$method" -min -scc -ff -s -R json  > "$out_dir/results.$method.sl.$i" 
#         find ../cyclist_graphs/cyclist_output_graphs/fo -maxdepth 1 -name "*.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -"$method" -min -scc -ff -s -R json  > "$out_dir/results.$method.fo.$i" 
#     done
# done
# for method in "SLA" "VLA"; do
#     for i in {1..3}; do
#         find ../cyclist_graphs/cyclist_output_graphs/sl -maxdepth 1 -name "*.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -"$method" -s -R json  > "$out_dir/results.$method.sl.$i" 
#         find ../cyclist_graphs/cyclist_output_graphs/fo -maxdepth 1 -name "*.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -"$method" -s -R json  > "$out_dir/results.$method.fo.$i" 
#     done
# done

for i in {1..3}; do
    find ../cyclist_graphs/cyclist_output_graphs/sl -maxdepth 1 -name "*.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -"SLA" -s -R json  > "$out_dir/results.SLA.sl.$i" 
    find ../cyclist_graphs/cyclist_output_graphs/fo -maxdepth 1 -name "*.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -"SLA" -s -R json  > "$out_dir/results.SLA.fo.$i" 
done

# for method in "SH" "OR" "FWK"; do
#     for i in {1..5}; do
#         dune exec src/generic/checkproof.exe -- -"$method" -min -scc -ff -s --json --unminimized-proofs < ../cyclist_graphs/unminimized/all_graphs.json  > "$out_dir/results.no_minimization.$method.$i"
#     done
# done

# for method in "VLA" "SLA"; do
#     for i in {1..2}; do
#         dune exec src/generic/checkproof.exe -- -"$method" -s --json < ../cyclist_graphs/unminimized/all_graphs.json  > "$out_dir/results.minimization."$method".$i"
#     done
# done
# for method in "VLA" "SLA"; do
#     for i in {1..2}; do
#         dune exec src/generic/checkproof.exe -- -"$method" -s --json --unminimized-proofs < ../cyclist_graphs/unminimized/all_graphs.json  > "$out_dir/results.no_minimization."$method".$i"
#     done
# done




# single

# cd ../
# out_dir="./checkproof_benchmarks/logs"
# dune exec src/generic/checkproof.exe -- -SH -min -scc -ff -s --unminimized-proofs --json < ../cyclist_graphs/unminimized/all_graphs.json  > "$out_dir/results.no_minimization.SH+DU.optimized_FC"