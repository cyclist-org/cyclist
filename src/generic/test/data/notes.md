# Notes on Test Cases in this Directory

## Graph 13

Hand-crafted heighted graph corresponding to Example 7.2.4 in James' thesis.
Demonstrates that we have to go around the loop twice to get back to the height
we started at.

Not SD, but is XSD.

## Graph 17

Heighted graph corresponding to proof found by Cyclist for
benchmarks/fo/13-2-hydra.tst when using SD, in commit f703c05.

Not ID.

## Graph 18

Heighted graph corresponding to proof found by Cyclist for
benchmarks/fo/13-2-hydra.tst when using Spot/relation-based, in commit f703c05.

This does satisfy SD.

It's not the "standard" proof of 2-Hydra.

## Graph 19

Hand-crafted heighted graph corresponding to the "standard" proof of 2-Hydra.

ID, but not SD or XSD.

## Graphs 20a and 20b

Heighted graphs corresponding to proofs found by Cyclist for
benchmarks/fo/12-r-example.tst and benchmarks/fo/13-2-hydra.tst, respectively,
when using XSD, in commit 6ee38be.

Not ID

# Graph 21

Heighted graph found during proof search for benchmarks/fo/05-p-and-q.tst that
makes XSD seg fault, in commit 8be7684.

# Graph 22

Heighted graph found during proof search for benchmarks/fo/13-2-hydra.tst.
This is a simpler example than graph_19 of a graph that is ID, but is neither
SD nor XSD.
