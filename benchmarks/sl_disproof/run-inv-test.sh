#!/bin/bash

# The Makefile overrides these; the fallbacks are for standalone use. Note that
# only the main dune instance can resolve executables, so PROVER must name the
# binary directly when several copies of this script run in parallel.
PROVER=${PROVER:-"dune exec sl_disprove --"}
TIMEOUT=${TIMEOUT:-60}
DEFS=${DEFS:-all.defs}
RESULTS=${RESULTS:-results}

QUERY=$1
NAME=$(echo "$QUERY" | md5sum | cut -f1 -d' ')
LOG="${RESULTS}/${NAME}.log"
OUT="${RESULTS}/${NAME}.out"

mkdir -p "$RESULTS"
exec > "$LOG"
exec 2>&1

export OCAMLRUNPARAM=b
$PROVER -s -t "$TIMEOUT" -IP -D "$DEFS" -S "$QUERY" > "$OUT" 2>&1

STATUS=$?
# 255 is an invalidity witness; 1 (no witness) and 2 (timeout) are both unknown.
if [ $STATUS -eq 255 ]; then
  PROVER_STATUS="sat"
else
  PROVER_STATUS="unknown"
fi

echo ============
echo -e "Query: $QUERY"
echo ============
echo -e "Status: ${PROVER_STATUS}"
echo ============
