mkdir ~/artifact/database
mkdir ~/artifact/database/fo
mkdir ~/artifact/database/sl

cd ~/artifact/cyclist

echo "generating FO sloped graphs..."
TST_OPTS="-OR -min -scc -ff --dump-graphs --unminimized-proofs -R json --graph-dir=~/artifact/database/fo -d" \
    make fo-tests \
    | pcregrep -o "(?<=Proof summary: ).*" \
    > ~/artifact/database/fo/summary.csv

echo "generating SL sloped graphs..."
TST_OPTS="-OR -min -scc -ff --dump-graphs --unminimized-proofs -R json --graph-dir=~/artifact/database/sl -d" \
    make sl-tests \
    | pcregrep -o "(?<=Proof summary: ).*" \
    > ~/artifact/database/sl/summary.csv