# Worst-case Benchmarks

These sequents elicit worst-case behaviour for the Cyclist prover.
They require the predicate definitions in the `examples/sl-permutation.defs` file.
The sequents are all provable, but require exponentially large proofs with many overlapping cycles.
When Cyclist is restricted to using only ancestral and/or non-bud nodes as back-link targets, then the checking of infinite descent for the pre-proofs found during proof search requires worst-case complexity.
