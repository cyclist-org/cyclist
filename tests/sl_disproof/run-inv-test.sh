#!/bin/bash

CMD=../../sl_disprove.native
QUERY=$1
NAME=$(echo "$QUERY" | md5sum | cut -f1 -d' ')
LOG="results/${NAME}.log"
OUT="results/${NAME}.out"
DEFS=all.defs

exec > "$LOG"
exec 2>&1

export OCAMLRUNPARAM=b
"$CMD" -s -t 60 -IP -D "$DEFS" -S "$QUERY" > "$OUT" 2>&1

STATUS=$?
if [ $STATUS -eq 0 ]; then 
  PROVER_STATUS="unsat"
elif [ $STATUS -eq 255 ]; then
  PROVER_STATUS="sat"
else
  PROVER_STATUS="unknown"
fi

echo ============
echo -e "Query: $QUERY" 
echo ============
echo -e "Status: ${PROVER_STATUS}"
echo ============

