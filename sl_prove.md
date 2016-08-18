---
layout: index
---
[Installation]: /installation
[APLAS12]: http://dx.doi.org/10.1007/978-3-642-35182-2_25
{:target="_blank"}

``sl_prove``
====================================================================================

``sl_prove`` is an automatic entailment prover for separation logic with inductive definitions.
The theory/design behind ``sl_prove`` appears in [[APLAS12]].

Compiling 
--------------------------------------------------------------------------------
If you are simply looking for the latest version of ``sl_prove``, have a look at [Installation].

The original version of ``sl_prove`` in [[APLAS12]] is not stored on GitHub.  To try the closest possible, 
you can checkout a very early commit.

~~~~~~~~~~
git clone https://github.com/ngorogiannis/cyclist.git cyclist
cd cyclist 
git checkout b289f39a6a46b4f57da3585aa7fd71f3ca8601fc
~~~~~~~~~~

Then consult the README included, which contains building instructions
relevant to that version.

Example
-----------------------------------
Here is an example. A linked-list segment from address ``x`` to address ``y`` 
can be defined as follows:

	ls { 
	    x=y => ls(x,y) | 
	    x!=y * x->x' * ls(x',y) => ls(x,y) 
	} 

This means that either the list segment is empty (hence ``x=y``) or it is not
empty (``x!=y``) and there is a cell allocated at ``x`` containing some value ``x'`` 
(``x->x'``) from which there is a list segment up to ``y`` (``ls(x',y)``).

Now we can ask whether, for instance, a chain of two segments from ``x`` through ``y`` to ``nil``
(``nil`` is a special, always non-allocatable address, much like C's' ``NULL``)
forms a proper list segment from ``x`` to ``nil``. This is how we pose the query
and the output we obtain:

	$ ./sl_prove.native -S "ls(x,y) * ls(y,nil) |- ls(x,nil)"
	Proved: ls^1(x, y) * ls^2(y, nil) |- ls^3(x, nil)

This means ``sl_prove`` found a proof for the above sequent.  Let's inspect this proof.

    $ ./sl_prove.native -S "ls(x,y)*ls(y,nil) |- ls(x,nil)" -p
    0: ls^1(x, y) * ls^2(y, nil) |- ls^3(x, nil) (ls L.Unf./Simplify) [1, 2]
      1: ls^2(x, nil) |- ls^3(x, nil) (Pred Intro) [3]
        3: emp |- emp (Id)
      2: nil!=x * y!=x * x->z * ls^2(y, nil) * ls^3(z, y) |- ls^3(x, nil) (ls R.Unf./Simplify) [4]
        4: nil!=x * y!=x * x->z * ls^2(y, nil) * ls^3(z, y) |- x->y' * ls^1(y', nil) (Pto Intro/Simplify) [5]
          5: nil!=x * y!=x * ls^2(y, nil) * ls^3(z, y) |- ls^1(z, nil) (Weaken) [6]
            6: ls^2(y, nil) * ls^3(z, y) |- ls^3(z, nil) (Subst) [7]
              7: ls^2(y, nil) * ls^3(x, y) |- ls^3(x, nil) (Backl) [0] <pre={(2, 2), (3, 1)}>

The ``-p`` argument lets us see the proof found. This proof is cyclic (node 7 is a back-link to node 0).
There are several other options, which are listed when ``sl_prove`` is run without arguments.

Benchmarks in [[APLAS12]]
----------------------------
To run a fixed set of entailment queries (including the sequents appearing in [[APLAS12]]), run

    $ make sl-tests

SL grammar
---------------------
The grammar for SL sequents is roughly as follows.

	sequent ::= form "|-" form
	form ::= heap | heap "\/" form
	heap ::= atomic | atomic "*" heap
	atomic ::= "emp" | eq | deq | pointsto | pred
	eq ::= term "=" term
	deq ::= term "!=" term
	pointsto ::= var "->" termlist
	termlist ::= term | term "," termlist
	term ::= var | "nil"
	pred ::= identifier "(" termlist ")"

where *var* matches any word over letters and numbers possibly postfixed by a
quote ', and *identifier* matches any word over letters and numbers.
Variables ending in a quote are implicitly existentially quantified.

