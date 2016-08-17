---
layout: index
---
[Installation]: /installation
[APLAS12]: http://dx.doi.org/10.1007/978-3-642-35182-2_25
{:target="_blank"}

``sl_prove``
====================================================================================

``sl_prove`` is an automatic entailment prover for separation logic with inductive definitions.

Here is an example. Suppose a list linked segment from address ``x`` to address ``y`` 
is defined as follows:

	ls { 
	    x=y => ls(x,y) | 
	    x!=y * x->x' * ls(x',y) => ls(x,y) 
	} 

This means that either the list segment is empty (hence ``x=y``) or it is not
empty (``x!=y``) and there is a cell allocated at ``x`` containing some value ``x'`` 
(``x->x'``) from which there is a list segment up to ``y`` (``ls(x',y)``).

Now we can ask whether a chain of list segments from ``x`` through ``y`` to ``nil``
(``nil`` is a special, always non-allocatable address, much like C's' ``NULL``)
forms a proper list segment from ``x`` to ``nil``. This is how we pose the query:

	$ ./sl_prove.native -S "ls(x,y) * ls(y,nil) |- ls(x,nil)"

The theory/design behind *Cyclist* and ``sl_prove`` in [[APLAS12]].

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
