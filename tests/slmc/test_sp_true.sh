#!/bin/bash

MC=../../sl_modelcheck.native

HEAP=

for i in `seq 1 $1`;
do
    if [ $i -gt 1 ]; then 
        HEAP="$HEAP,"
    fi
    HEAP="$HEAP 0x$i |-> ($i)"
    (set -x; time $MC -D ../../examples/sl.defs -F "spTrue()" -M "([], [ $HEAP ])")
done
