Cyclist
================

[![Build](https://github.com/cyclist-org/cyclist/actions/workflows/build.yml/badge.svg)](https://github.com/cyclist-org/cyclist/actions/workflows/build.yml)

*Cyclist* is a framework for building cyclic theorem provers based on a sequent calculus.
In addition, over the years several decision procedures or algorithms have been integrated,
focusing on Separation Logic with inductively defined predicates.

All of the provers are subcommands of a single `cyclist` binary:

```
cyclist sl prove|disprove|modelcheck|satcheck|satexpgen
cyclist while prove|abduce
cyclist asl-while prove
cyclist proc prove
cyclist fo prove
cyclist ltl prove
cyclist checkproof
```

Run `cyclist --help` for the command tree, or `cyclist COMMAND --help` for the
options of a particular prover.

For information on building *Cyclist*, the available tools included and the papers underpinning those tools, visit

[www.cyclist-prover.org](http://www.cyclist-prover.org)
