#!/bin/bash

cat seqs | tr '\n' '\0' | nice xargs -n 1 -P 4 -0 ./run-inv-test.sh
