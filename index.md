---
layout: index
---

The Cyclist Prover
=======================================

``Cyclist`` is two things:

* A generic, cyclic theorem prover *framework* written in OCaml and C++. 

* A collection of *provers* and other tools, with a focus on *Separation Logic with inductive definitions*, hereafter **SL**.


Actively maintained tools in Cyclist
---------------------------------------

1. ``sl_prove`` is a prover for entailments in SL.

2. ``sl_satcheck`` implements a decision procedure for satisfiability of SL formulas.

3. ``sl_disprove`` is a heuristic for *disproving* entailments in SL.

4. ``sl_modelcheck`` decides whether the given SL formula is satisfied by a provided program state.

5. ``while_prove``: A termination and/or safety prover for simple while-programs with pointers 
   and dynamic allocation specified by SL formulas.

6. ``while_abduce``: A prover like ``while_prove``, but which attempts
   to infer a suitable precondition in SL such that the target
   program will terminate/execute safely, *including inferring the 
   inductive definitions themselves*.

No longer maintained tools
-----------------------------------------

1. ``fo_prove``: A cyclic prover for (for all-exists) first order logic with
   inductive definitions.

2. ``goto_prove``: A cyclic prover for a program logic like while_prove
   above, but for a go-to language.


