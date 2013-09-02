#!/bin/sh

TIME_TAKEN=$(mktemp)
SEARCH_DEPTH=$(mktemp)
#PROOF_SIZE=$(mktemp)
BACKLINKS=$(mktemp)
#UNSOUND=$(mktemp)
#MODCHECK_CALLS=$(mktemp)

grep "Elapsed CPU time:" $1 | cut -f 4 -d\ > $TIME_TAKEN
grep "Proof has" $1 | cut -f 9 -d \ | tr -d . > $SEARCH_DEPTH
#grep "Proof has" $1 | cut -f 3 -d \ > $PROOF_SIZE
grep "Proof has" $1 | cut -f 11 -d \ > $BACKLINKS
#grep "Model checker calls: Rejected" $1 | cut -f 5 -d \ > $UNSOUND
#grep "Model checker calls: Rejected" $1 | cut -f 8 -d \ > $MODCHECK_CALLS

paste -d "&" abd-table-mutant.tex $TIME_TAKEN $SEARCH_DEPTH $BACKLINKS | awk '{print $0, " & \\checkmark \\\\"}'

rm -f $TIME_TAKEN $SEARCH_DEPTH $BACKLINKS

