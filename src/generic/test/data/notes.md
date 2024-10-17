# Notes on Test Cases in this Directory

## Graph 13

Hand-crafted heighted graph corresponding to Example 7.2.4 in James' thesis.
Demonstrates that we have to go around the loop twice to get back to the height
we started at.

Not SD, but is XSD.
Does not satisfy TM or FC, but does satisfy DU.

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

# Graph 23

A degenerate example, with one node and no edges. Added to check against bug in
SLA code found in c94abc9.

# Graphs 24 and 25

Hand-crafted examples to demonstrate the difference in the sizes of the full
and order-reduced composition closures. Both satisfy ID.

Graph 25 demonstrates a bug in the implementation of Tarjan's SCC algorithm in
sloped_relation.c discovered in commit fd2546f, which incorrectly reports that
this example is UNSOUND (i.e. does NOT satisfy ID).

# Graphs 26 to 31

Hand-crafted examples to test the implementation of the Trace Manifold condition
(in commit 9881afe).

Graphs 27 and 28 have the same underlying directed graph.
Graphs 29 and 30 have the same underlying directed graph.

* Graph 26: a variation of graph 13 that adds a trace loop, making the trace manifold graph non-empty. Does not satisfy TM, but satisfies FC and ID.
* Graph 27: has two overlapping cycles with connected traces, only one of which is descending. Does not satisfy TM or ID.
* Graph 28: has two overlapping cycles, but the descending trace of one of the cycles is disconnected from the others. Does not satisfy TM because the descending trace for bud 2 is only connected to a trace for bud 3 via another (non-descending) trace for bud 2, but does satisfy ID.
* Graph 29: has three overlapping cycles. This was designed to verify that the implementation does not incorrectly test for traces between the bud set {2,5}: these two cycles are connected, but only via the cycle of bud 4, and so do not represent a connected induced subgraph of the structural connectivity relation. Non-descending traces for cycles 2 and 5 are connected via a descending trace for cycle 4. Does satisfy TM (and so satisfies ID).
* Graph 30: has three overlapping cycles. Does not satisfy ID. Does not satisfy TM, because only non-descending traces for the collection of all three cycles are connected in the trace manifold graph.
* Graph 31: has three overlapping cycles. Does not satisfy ID. Does not satisfy TM because for the cycle set {2,4}, although there is a path between descending traces these cycles via a (descending) trace for cycle 5, there is not an edge between these two traces in the trace manifold graph. However, there is an edge in the trace manifold graph between two *other* non-descending traces from cycle 4 to cycle 2, so an implementation that looked for connecting edges outside of the induced trace manifold subgraph would incorrectly report that this graph satisfies TM.
