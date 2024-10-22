mkdir /home/database
mkdir /home/database/fo
mkdir /home/database/sl

cd /home/cyclist

echo "generating FO sloped graphs..."
TST_OPTS="-OR -min -scc -ff --dump-graphs --unminimized-proofs -R json --graph-dir=/home/database/fo -d" \
    make fo-tests \
    | grep -o "(?<=Proof summary: ).*" \
    > /home/database/fo/summary.csv

echo "generating SL sloped graphs..."
TST_OPTS="-OR -min -scc -ff --dump-graphs --unminimized-proofs -R json --graph-dir=/home/database/sl -d" \
    make sl-tests \
    | grep -o "(?<=Proof summary: ).*" \
    > /home/database/sl/summary.csv