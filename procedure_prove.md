---
layout: index
---
[Github]: https://www.github.com/ngorogiannis/cyclist
{:target="_blank"}
[Termination Problems Database]: http://termination-portal.org/wiki/TPDB
{:target="_blank"}

``procedure_prove``
=============================================================
``procedure_prove`` is a termination and/or safety prover for simple 
procedural imperative programs. It shares its program syntax with the 
``while_prove`` prover, but also supports procedures, which may be recursive, 
that have a pass-by-value semantics for parameters. Procedure specifications in 
the form of pre-/postcondition pairs must be given, where pre- and 
postconditions are given by SL formulas.

The prover comes with a benchmark suite of around 40 test programs. This 
comprises around 20 custom benchmarks, as well as around 20 examples taken from 
the ``Java_bytecode_recursive`` section of the 
[Termination Problems Database].

Compiling & Running
----------------------------------------------------
After downloading the source code from [Github], follow the 
[installation instructions](installation) and compile by running ``make``. This
will produce an executable called ``procedure_prove.native``. 

Running ``procedure_prove.native`` without options will produce some help 
text explaining its usage. You can specify on the command line which procedures 
you wish to be analysed. If no procedures are given on the command line, the 
tool will look for a ``main`` procedure. You can specify that all procedures in
the input file should be analysed by giving the command-line option ``-all``.
By default, the prover will analyse an input program for memory safe execution 
only; if you wish the prover to also prove termination of the program, run the 
executable with the command-line option ``-T``.

Example
--------------
The following program (included in the benchmark suite as ``23-cls-rec.wl2``)
traverses a cyclic linked list.

	fields: next;
	
	proc traverselist(x, end)
	  precondition: ls(x, end);
	  postcondition: ls(x, end);
	{
	  if x != end then
	  
	    y := x.next;
	    traverselist(y, end)
	  
	  fi
	}
	
	proc main(x)
	  precondition: cls(x);
	  postcondition: cls(x);
	{
	  y := x.next;
	  traverselist(y, x)
	}

The first line declares that heap cells are records with one field, called 
``next`` (this program is for use with linked lists).

The program consists of two procedures, ``traverselist`` and ``main``, each 
with a pre-/postcondition specification. The specification of ``traverselist``
asserts that the precondition for (safely) calling the procedure is that the 
memory is described by the inductive predicate ``ls``, whose definition is as 
follows (see [``sl_prove``](sl_prove) for details on how to interpret the definition):

	ls { 
		x=y => ls(x,y) | 
		x!=y * x->x' * ls(x',y) => ls(x,y) 
	}

That is, the memory must contain a linked list segment whose head is referenced
by its first parameter ``x``, and whose last element is referenced by the 
second parameter ``end``. The specification of the ``main`` procedure is that 
its parameter ``x`` must reference a cyclic linked list, which is expressed by 
the predicate ``cls`` whose definition is as follows:

	cls {
		x->y' * ls(y',x) => cls(x) 
	}

Let's verify that this program *terminates safely* (i.e., without committing 
memory faults).

<pre>
$ ./procedure_prove.native -T -P benchmarks/procs/23-cls-rec.wl2 -p
0:  |- {ls[a](x, end)} traverselist(x, end) {ls[a'](x, end)} (Proc Unf. traverselist) [1 <{(a, a)}/{}>]
  1:  |- {ls[a](x, end)} 0: if x!=end then 1: ... fi {ls[a'](x, end)} (If) [2 <{(a, a)}/{}>, 3 <{(a, a)}/{}>]
    2:  |- {x!=end * ls[a](x, end)} 1: y := x.next; 2: ... {ls[a'](x, end)} (ls L.Unf.) [4 <{(a, a)}/{(a, b')}>, 5 <{(a, a)}/{(a, b')}>]
      4:  |- {[b' < a] : x=end * x!=end} 1: y := x.next; 2: ... {ls[a'](x, end)} (Ex Falso)
      5:  |- {[b' < a] : nil!=x * x!=end * x->w' * ls[b'](w', end)} 1: y := x.next; 2: ... {ls[a'](x, end)} (Load/Simplify) [6 <{(b', b'), (a, a)}/{}>]
        6:  |- {[b' < a] : nil!=x * x!=end * x->y * ls[b'](y, end)} 2: traverselist(y, end) {ls[a'](x, end)} (Ex.Intro.) [7 <{(b', b), (a, a)}/{}>]
          7:  |- {[b < a] : nil!=x * x!=end * x->y * ls[b](y, end)} 2: traverselist(y, end) {ls[a'](x, end)} (RHS.Conseq) [8 <{(b, b), (a, a)}/{}>]
            8:  |- {[b < a] : nil!=x * x!=end * x->y * ls[b](y, end)} traverselist(y, end) {[b < a] : nil!=x * x!=end * x->y * ls[a'](y, end)} (Frame) [9 <{(b, b)}/{}>]
              9:  |- {ls[b](y, end)} traverselist(y, end) {ls[a'](y, end)} (Subst) [10 <{(b, a)}/{}>]
                10:  |- {ls[a](y, end)} traverselist(y, end) {ls[a'](y, end)} (Param Subst) [11 <{(a, a)}/{}>]
                  11:  |- {ls[a](x, end)} traverselist(x, end) {ls[a'](x, end)} (Backl) [0] <pre={(a, a)}>
    3:  |- {x=end * ls[a](x, end)}  {ls[a'](x, end)} (Axiom)

0:  |- {cls[a](x)} main(x) {cls[a'](x)} (Proc Unf. main) [1 <{(a, a)}/{}>]
  1:  |- {cls[a](x)} 0: y := x.next; 1: ... {cls[a'](x)} (cls L.Unf.) [2 <{(a, a)}/{(a, b')}>]
    2:  |- {[b' < a] : nil!=x * x->w' * ls[b'](w', x)} 0: y := x.next; 1: ... {cls[a'](x)} (Load/Simplify) [3 <{(b', b'), (a, a)}/{}>]
      3:  |- {[b' < a] : nil!=x * x->y * ls[b'](y, x)} 1: traverselist(y, x) {cls[a'](x)} (Ex.Intro.) [4 <{(b', b), (a, a)}/{}>]
        4:  |- {[b < a] : nil!=x * x->y * ls[b](y, x)} 1: traverselist(y, x) {cls[a'](x)} (RHS.Conseq) [5 <{(b, b), (a, a)}/{}>]
          5:  |- {[b < a] : nil!=x * x->y * ls[b](y, x)} traverselist(y, x) {[b < a] : nil!=x * x->y * ls[a'](y, x)} (Frame) [6 <{(b, b)}/{}>]
            6:  |- {ls[b](y, x)} traverselist(y, x) {ls[a'](y, x)} (Subst) [7 <{(b, a)}/{}>]
              7:  |- {ls[a](y, x)} traverselist(y, x) {ls[a'](y, x)} (Param Subst) [8 <{(a, a)}/{}>]
                8:  |- {ls[a](x, end)} traverselist(x, end) {ls[a'](x, end)} (Proc Unf. traverselist) [9 <{(a, a)}/{}>]
                ...
</pre>

The output shows (textual representations of) *cyclic* termination proofs for 
the two procedures. Notice that the ``traverselist`` procedure is analysed 
before ``main`` since ``main`` depends on (i.e. makes a call to) 
``traverselist``. Thus, the proof of ``main`` actually includes the proof of 
``traverselist`` as a sub-proof, however we elide this in the output above. 
The recursion in the ``traverselist`` procedure is handled in the cyclic proof
by a *back-link*, which can be seen in node 11 of the proof above.

Each predicate is marked with a variable, e.g. ``ls[a](x, end)``. At points in
the proof where the analysis splits on each case of the predicate definition 
(e.g. at node 2 in the first proof above), newly introduced predicates are given 
fresh variables and constraints are introduced between these fresh variables and 
that of the unfolded predicate, e.g. ``[b' < a]``. Cyclist uses these 
variables to construct termination measures for programs, and the constraints 
indicate when these measures decrease. The proof above indicates how the 
measures track from one node to the next. For example, in the first proof above,
node 2 has two successors: nodes 4 and 5. We can track the measure from variable
``a`` in node 2 to variables ``a`` and ``b'`` in each of its successors. 
Moreover, when we track the measure from ``a`` to ``b'`` the termination 
measure decreases.

In this example, the termination measure tracked by Cyclist coincides with the 
length of the linked list: each (recursive) call to the ``traverselist``
procedure moves one link along the linked list. Cyclist verifies termination by
ensuring that some termination measure decreases in every cycle in the proof.

Benchmarks
----------------------------------------------------
To run the benchmarks test do

    $ make proc-tests

To obtain more information from the tests, the shell variable ``TST_OPTS`` 
will add command-line arguments to ``procedure_prove``. For example, the 
command

    TST_OPTS=-s make proc-tests

will produce statistics for each test, including runtimes.
