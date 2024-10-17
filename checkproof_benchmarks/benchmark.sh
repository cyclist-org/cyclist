

cd ../
out_dir="./checkproof_benchmarks/logs"

fo_graphs_dir="../cyclist_graphs/cyclist_output_graphs/fo"
sl_graphs_dir="../cyclist_graphs/cyclist_output_graphs/sl"

timeout_seconds=3

run_method() {
    method="$1"
    flags="$2"
    iterations=$3
    out_directory="$4"
    minimized=$5

    for i in $(seq 1 $iterations); do
        echo "running $method $i..."
        if [ "$minimized" = true ]; then
            find ../cyclist_graphs/cyclist_output_graphs/sl/minimized -maxdepth 1 -name "*.minimised.json" | sort | xargs cat | dune exec src/generic/checkproof.exe -- -$method $flags -s -R json  > "$out_directory/results.$method.sl.$i" 
            find ../cyclist_graphs/cyclist_output_graphs/fo/minimized -maxdepth 1 -name "*.minimised.json" | sort | xargs cat | dune exec src/generic/checkproof.exe -- -$method $flags -s -R json  > "$out_directory/results.$method.fo.$i" 
            # find ../cyclist_graphs/cyclist_output_graphs/sl/minimized -maxdepth 1 -name "*.minimised.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -$method $flags -s -R json  > "$out_directory/results.$method.sl.$i" 
            # find ../cyclist_graphs/cyclist_output_graphs/fo/minimized -maxdepth 1 -name "*.minimised.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -$method $flags -s -R json  > "$out_directory/results.$method.fo.$i" 
        else
            # sl_out_filepath="$out_directory/results.$method.sl.$i" 
            # find ../cyclist_graphs/cyclist_output_graphs/sl -maxdepth 1 -name "*.json" | sort | xargs -S1024 -I {} bash -c \
            # 'timeout $2 ./_build/install/default/bin/checkproof -$3 $4 -f $1 -s -R json || echo "MODCHECK: Absolute time spent model checking: TIMEOUT ms"' \
            # -- {} "$timeout_seconds" "$method" "$flags" "$sl_out_filepath" \
            # > "$sl_out_filepath"
            
            # find ../cyclist_graphs/cyclist_output_graphs/fo -maxdepth 1 -name "*.json" | sort | xargs -I {} ./_build/install/default/bin/checkproof -$method $flags -f {} -s -R json  > "$out_directory/results.$method.fo.$i" 

            # find ../cyclist_graphs/cyclist_output_graphs/sl -maxdepth 1 -name "*.json" | sort | xargs -I {} ./_build/install/default/bin/checkproof -$method $flags -f {} -s -R json  > "$out_directory/results.$method.sl.$i" 
            # find ../cyclist_graphs/cyclist_output_graphs/fo -maxdepth 1 -name "*.json" | sort | xargs -I {} ./_build/install/default/bin/checkproof -$method $flags -f {} -s -R json  > "$out_directory/results.$method.fo.$i" 
            # find ../cyclist_graphs/cyclist_output_graphs/sl -maxdepth 1 -name "*.json" | sort | xargs -I {} dune exec src/generic/checkproof.exe -- -$method $flags -f {} -s -R json  > "$out_directory/results.$method.sl.$i" 
            # find ../cyclist_graphs/cyclist_output_graphs/fo -maxdepth 1 -name "*.json" | sort | xargs -I {} dune exec src/generic/checkproof.exe -- -$method $flags -f {} -s -R json  > "$out_directory/results.$method.fo.$i" 
            find ../cyclist_graphs/cyclist_output_graphs/sl -maxdepth 1 -name "*.json" | sort | xargs cat | dune exec src/generic/checkproof.exe -- -$method $flags -s -R json  > "$out_directory/results.$method.tm.sl.$i" 
            find ../cyclist_graphs/cyclist_output_graphs/fo -maxdepth 1 -name "*.json" | sort | xargs cat | dune exec src/generic/checkproof.exe -- -$method $flags -s -R json  > "$out_directory/results.$method.tm.fo.$i" 
        fi
        echo done $method $i
    done
}

run_SLA_in_parallel() {
    flags="$1"
    iterations=$2
    out_directory="$3"
    method="SLA"
    for i in $(seq 1 $iterations); do
        echo "running SLA $i..."

        find ../cyclist_graphs/cyclist_output_graphs/fo -maxdepth 1 -name "*.json" | sort | xargs -S1024 -I {} bash -c \
            'timeout $2 ./_build/install/default/bin/checkproof -$3 $4 -f {} -s -R json || echo "MODCHECK: Absolute time spent model checking: TIMEOUT ms"' \
            -- {} "$timeout_seconds" "$method" "$flags" "$sl_out_filepath" \
            > $out_directory/results.SLA.fo.$i

        sl_out_filepath="$out_directory/results.SLA.sl.$i" 
        graphs_files_names=$(find ../cyclist_graphs/cyclist_output_graphs/sl -maxdepth 1 -name "*.json" | sort)
        echo "$graphs_files_names" | split -l $(($(echo "$graphs_files_names" | wc -l)/3+1)) -d - "graphs_file_names_part_"

        for j in {0..2}; do
            cat graphs_file_names_part_0$j \
            | xargs -S1024 -I {} bash -c \
            'timeout $2 ./_build/install/default/bin/checkproof -$3 $4 -f {} -s -R json || echo "MODCHECK: Absolute time spent model checking: TIMEOUT ms"' \
            -- {} "$timeout_seconds" "$method" "$flags" "$sl_out_filepath" \
            > $out_directory/results.SLA.sl.part.$j &
        done

        wait
        cat $out_directory/results.SLA.sl.part.0 $out_directory/results.SLA.sl.part.1 $out_directory/results.SLA.sl.part.2 > $sl_out_filepath
        echo done SLA $i
    done       
}

# find ../cyclist_graphs/cyclist_output_graphs/sl -maxdepth 1 -name "*.json" | sort | xargs -I {} echo -f {} | xargs dune exec src/generic/checkproof.exe -- -FWK -min -scc -ff --unminimized-proofs -s -R json  > "./checkproof_benchmarks/logs/non-minimized/results.deleteme.FWK.sl.2" 

# run_method "SH" "-min -scc -ff --unminimized-proofs" 5 "$out_dir/minimized" true
# run_method "OR" "-min -scc -ff --unminimized-proofs" 5 "$out_dir/minimized" true
# run_method "FWK" "-min -scc -ff --unminimized-proofs" 5 "$out_dir/minimized" true
# run_method "VLA" "-min -scc -ff --unminimized-proofs" 5 "$out_dir/minimized" true
# run_method "SLA" "-min -scc -ff --unminimized-proofs" 5 "$out_dir/minimized" true

# run_method "SH" "-min -scc -ff --unminimized-proofs" 5 "$out_dir/non-minimized" false
# run_method "OR" "-min -scc -ff --unminimized-proofs" 5 "$out_dir/non-minimized" false
# run_method "FWK" "-min -scc -ff --unminimized-proofs" 3 "$out_dir/non-minimized" false
# run_method "VLA" "-min -scc -ff --unminimized-proofs" 3 "$out_dir/non-minimized" false
# run_method "SLA" "-min -scc -ff --unminimized-proofs" 1 "$out_dir/non-minimized" false
# run_SLA_in_parallel "-min -scc -ff --unminimized-proofs" 1 "$out_dir/non-minimized"