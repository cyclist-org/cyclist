---
layout: index
---
[Installation]: /installation
[APLAS12 paper]: http://dx.doi.org/10.1007/978-3-642-35182-2_25
{:target="_blank"}

An entailment prover for a fragment of first-order logic with inductive definitions.
====================================================================================

OVERVIEW:
--------------------------------------------------------------------------------

Cyclist is a framework for building cyclic theorem provers based on a sequent
calculus.

Three provers were included in the [APLAS12 paper].
These are (binaries end in ".native" omitted below):

fo_prove
: An entailment prover for a fragment of first-order logic with inductive
definitions (definitions needed for the test cases in the paper are in
examples/fo.defs).

sl_prove
: An entailment prover for a fragment of separation logic with inductive
definitions (those used in the tests are in examples/sl.defs).

goto_prove
: A termination prover for a heap-manipulating, goto-like programming language
with specifications in the above fragment of separation logic.

THEORY:
--------------------------------------------------------------------------------

The theory/design behind Cyclist and the three provers above is described in the [APLAS12 paper].

>  J. Brotherston, N. Gorogiannis, and R. L. Petersen. A generic cyclic theorem
>  prover. In Proc. APLAS-10, pages 350-367. Springer, 2012.

The grammar for SL sequents is roughly as follows.

	sequent ::= form "|-" form
	form ::= heap | heap "\/" form
	heap ::= atomic | atomic "*" heap
	atomic ::= "emp" | "true" | eq | deq | pointsto | pred
	eq ::= term "=" term
	deq ::= term "!=" term
	pointsto ::= var "->" termlist
	termlist ::= term | term "," termlist
	term ::= var | "nil"
	pred ::= identifier "(" termlist ")"

where *var* matches any word over letters and numbers possibly postfixed by a
quote ', and *identifier* matches any word over letters and numbers.

QUICKSTART:
--------------------------------------------------------------------------------

If you want to use the separation logic prover and you downloaded a tarball then 
the executables in the archive will have been linked in such a way that they 
should work without any additional dependencies on an x86_64 Linux system.
Otherwise consult [Installation] for building instructions.

As the two other provers are not under active development, if you want to try
them out your best option is to check out an older revision:

~~~~~~~~~~
git clone https://github.com/ngorogiannis/cyclist.git cyclist
cd cyclist 
git checkout b289f39a6a46b4f57da3585aa7fd71f3ca8601fc
~~~~~~~~~~

Then consult the README of that revision, it contains building instructions
relevant to that version.