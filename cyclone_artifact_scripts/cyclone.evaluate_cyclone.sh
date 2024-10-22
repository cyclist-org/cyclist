timeout_seconds=3

run_method() {
    method="$1"
    flags="$2"
    out_directory="$3"

    echo "running $method..."
    find /home/database/fo -maxdepth 1 -name "*.json" | sort | xargs cat | dune exec src/generic/checkproof.exe -- -$method $flags -s -R json  > "$out_directory/$method.output.fo" 
    find /home/database/sl -maxdepth 1 -name "*.json" | sort | xargs cat | dune exec src/generic/checkproof.exe -- -$method $flags -s -R json  > "$out_directory/$method.output.sl" 
    echo done $method
}


# methods=("CY" "OR" "FWK" "VLA" "SLA")
methods=("CY" "OR" "FWK" "VLA")
cd /home/cyclist
for method in "${methods[@]}" ; do
    run_method "$method" "-min -scc -ff --unminimized-proofs" "/home"
done


stats_file_path="/home/stats.csv"

tail -n+2 $stats_file_path | cut -d ',' -f2,5 > /home/evaluation.body

for method in "${methods[@]}" ; do
    cat /home/$method.output.fo | grep -o -P "(?<=MODCHECK: Absolute time spent model checking: )[^\s]+" > "$method.modelcheck.fo.csv"
    cat /home/$method.output.sl | grep -o -P "(?<=MODCHECK: Absolute time spent model checking: )[^\s]+" > "$method.modelcheck.sl.csv"
    cat $method.modelcheck.fo.csv $method.modelcheck.sl.csv > $method.modelcheck.database.csv
    rm "$method.modelcheck.fo.csv" "$method.modelcheck.sl.csv"

    paste -d ',' /home/evaluation.body "$method.modelcheck.database.csv" > /home/evaluation.body.tmp
    rm "$method.modelcheck.database.csv"
    mv /home/evaluation.body.tmp /home/evaluation.body
done

echo "edges,CY,OR,FWK,VLA,SLA" > evaluation.header
cat evaluation.header /home/evaluation.body > /home/evaluation.csv
rm evaluation.header
awk -F',' '$NF != 0.000000' /home/evaluation.csv > /home/evaluation.csv.temp
mv /home/evaluation.csv.temp /home/evaluation.csv