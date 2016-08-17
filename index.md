---
layout: index
---

The *Cyclist* Framework and Provers
=======================================

*Cyclist* is two things:

* A generic, cyclic theorem prover *framework*. It can be used for constructing
theorem provers that employ *cyclic proof* (usually, in order to treat 
inductively defined predicates) for practically any logic.

* A collection of theorem provers and other tools, with a focus on 
*Separation Logic with inductive definitions*, hereafter abbreviated as **SL**.
Checkout the [**demo**](demo) for a live demonstration of the ``sl_prove`` tool.

*Cyclist* is written in OCaml and C++. 

Actively maintained tools in *Cyclist*
---------------------------------------

* [``sl_prove``](sl_prove) is a prover for entailments in SL.
See the [**demo**](demo) for a live demonstration.

* [``sl_satcheck``](sl_satcheck) implements a decision procedure for satisfiability of SL formulas.

* [``sl_disprove``](sl_disprove) is a heuristic for *disproving* entailments in SL.

* [``sl_modelcheck``](sl_modelcheck) decides whether the given SL formula is satisfied by a provided program state.

* [``while_prove``](while_abduce) is termination and/or safety prover for simple while-programs with pointers 
   and dynamic allocation specified by SL formulas.

* [``while_abduce``](while_abduce) is a prover like [``while_prove``](while_abduce), but which attempts
   to infer a suitable precondition in SL such that the target
   program will terminate/execute safely, *including inferring the 
   inductive definitions themselves*.

No longer maintained tools
-----------------------------------------

* ``fo_prove``: A cyclic prover for (for all-exists) first order logic with
   inductive definitions.

* ``goto_prove``: A cyclic prover for a program logic like while_prove
   above, but for a go-to language.


