timeout_seconds=3

run_method() {
    method="$1"
    flags="$2"
    out_directory="$3"

    echo "running $method $i..."
        find /home/database/fo -maxdepth 1 -name "*.json" | sort | xargs cat | timeout $timeout_seconds dune exec /home/cyclist/src/generic/checkproof.exe -- -$method $flags -s -R json  > "$out_directory/$method.output.fo" 
        find /home/database/sl -maxdepth 1 -name "*.json" | sort | xargs cat | timeout $timeout_seconds dune exec /home/cyclist/src/generic/checkproof.exe -- -$method $flags -s -R json  > "$out_directory/$method.output.sl" 
    echo done $method $i
}


out_dir="$HOME/artifact"
methods=("CY" "OR" "FWK" "VLA" "SLA")
cd /home/cyclist
dune clean
dune build
cd /home/scripts
# for method in "${methods[@]}" ; do
#     run_method "$method" "-min -scc -ff --unminimized-proofs" "$out_dir"
# done


stats_file_path="$HOME/artifact/stats.csv" # TODO: change

tail -n+2 $stats_file_path | cut -d ',' -f5 > /home/evaluation.body

for method in "${methods[@]}" ; do
    cat $out_dir/$method.output.fo | pcregrep -o "(?<=MODCHECK: Absolute time spent model checking: )[^\s]+" > "$method.modelcheck.fo.csv"
    cat $out_dir/$method.output.sl | pcregrep -o "(?<=MODCHECK: Absolute time spent model checking: )[^\s]+" > "$method.modelcheck.sl.csv"
    cat $method.modelcheck.fo.csv $method.modelcheck.sl.csv > $method.modelcheck.database.csv
    rm "$method.modelcheck.fo.csv" "$method.modelcheck.sl.csv"

    paste -d ',' /home/evaluation.body "$method.modelcheck.database.csv" > /home/evaluation.body.tmp
    rm "$method.modelcheck.database.csv"
    mv /home/evaluation.body.tmp /home/evaluation.body
done

echo "edges,CY,OR,FWK,VLA,SLA" > evaluation.header
cat evaluation.header /home/evaluation.body > /home/evaluation.csv
rm evaluation.header