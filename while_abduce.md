---
layout: index
---
[Installation]: /installation
[SAS14]: http://dx.doi.org/10.1007/978-3-319-10936-7_5
{:target="_blank"}

``while_abduce``
=============================================================
``while_abduce``
is a termination and/or safety *abducer* for simple while-programs.
The tool and the theory behind it are presented in the [[SAS14]] paper.

The goal of the tool is the same with that of [``while_prove``](while_prove)
with the crucial difference that the precondition is *not given* and must
be found by the tool, *including the inductive definitions of all predicates used*.

Compiling
----------------------------------------------------
If you are simply looking for the latest version of ``while_abduce``, have a look at [Installation].

If you want to try out the original version underpinning the [[SAS14]] paper, there are two options.

1. You can download an archive of the sources and the original benchmark suite at the 
   [SAS14 GitHub release](https://github.com/ngorogiannis/cyclist/releases/tag/SAS14). 
   You will then have to build the tool according to the instructions contained within.
2. You can download a 
   [virtual machine](http://cs.uni-muenster.de/sev/sas14/artifacts.php)
   virtual machine from the SAS-14 website. You can run this VM using Virtual Box.

Running ``while_abduce.native`` without options will produce some help text explaining its usage.

Example
----------------
Consider the following program (see [``while_prove``](while_prove) for info on the syntax).

    fields: next;
    precondition: cls(x) ;
    y := x;
    x := x.next;
    while x!=y do 
      x := x.next 
    od

NB that the definition of ``cls(x)`` in ``examples/sl.defs`` is **not** consulted. The prover treats ``cls`` 
as an as-yet undefined predicate. Let's try the abducer.

    $ ./while_abduce.native -T -sd -P benchmarks/whl_abd/09-cyclic-list.wl 
    Proved: cls^1(x) |- y := x; ...
    cls { 
    nil!=z * z->x' * I001^1(z, x') => cls(z)
    } ; 
    
    I001 { 
    w=z => I001(z,w) | 
    nil!=w * w!=z * w->y' * I001^1(z, y') => I001(z,w)
    }

We require synthesising a definition for ``cls`` that ensures safe termination (``-T``) and
want to see an equivalent, human-readable version (``-sd``, see [[SAS14]] for an explanation).

The abducer produces a definition for ``cls`` (and auxiliary recursive predicate ``I001``) that
amounts to a cyclic list. We can also extract the cyclic termination proof with ``-p`` as in ``while_prove``.


Benchmarks in [[SAS14]]
----------------------------------------------------
The command 

	make whl_abd-tests

will run the tests discussed in the paper.

*NB the Mutant tests were made available to us with permission
and are not included in this image.*

To obtain more information from the tests, the shell variable
TST_OPTS will add command-line arguments to while_abduce.native.
For example, the command

	TST_OPTS=-s make whl_abd-tests

will produce statistics for each test, including runtimes.
Similarly, the command

	TST_OPTS=-sd make whl_abd-tests

will display the simplified inductive definitions abduced for
each test.  Other command-line arguments include producing
LaTeX code for proofs or definitions, displaying proofs and
altering the search parameters.
