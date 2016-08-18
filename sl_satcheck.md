---
layout: index
---
[CSL-LICS14]: http://dx.doi.org/10.1145/2603088.2603091
{:target="_blank"}

``sl_satcheck``
=======================================================================

The ``sl_satcheck`` tool implements a decision procedure for 
satisfiability of separation logic formulas with inductive definitions.
The algorithm, its correctness and its complexity are discussed in the [[CSL-LICS14]] paper.


Compiling
-----------------------------------
If you are simply looking for the latest version of ``sl_satcheck``, have a look at [Installation](installation).

If you want to try out the original version underpinning the [[CSL-LICS14]] paper, then you can
download an archive including x86_64 binaries and the original benchmark suite at the 
[CSL-LICS14 GitHub release](https://github.com/ngorogiannis/cyclist/releases/tag/CSL-LICS14).
Bear in mind we changed the name of the tool from ``slsat_check`` to ``sl_satcheck`` after 
publication; the instructions below are for the current version, so you need to use the old name
if you are using the release archive.

Running

    $ sl_satcheck.native -h

will print out instructions for use.

Example
------------------
(See [``sl_prove``](sl_prove) for the definition of a linked-list segment).

In SL the formula ``x->y * x->y`` is unsatisfiable because ``*`` forces the two conjuncts to occupy disjoint parts of the heap.
How about ``ls(x,y) * ls(x,y)``?  Create a file called ``query`` with the following contents.

    P {
        ls(x,y) * ls(x,y) => P(x,y)
    };
    
	ls { 
	    x=y => ls(x,y) | 
	    x!=y * x->x' * ls(x',y) => ls(x,y) 
	} 

Then issue the query.

    $ sl_satcheck.native -D query
    SAT: First predicate has a non-empty base.

Which means that the formula ``ls(x,y) * ls(x,y)`` is satisfiable (when ``x=y``).

Benchmarks in [[CSL-LICS14]]
-------------------------------------
1. A superset of the hand-written tests described in the paper (Sec. 5.1) are 
   in ``examples/sl.defs``.  To check their satisfiability, run

    $ ./sl_satcheck.native

   The worst-case complexity example is in ``benchmarks/slsat/cc/succ-circuit*.defs``.
   To run this test iterate the checker over these files, i.e.,

   ~~~~~~~~
   for F in benchmarks/cc/succ-circuit[1-7].defs ; do
    	./sl_satcheck.native -s -D $F -t 2400;
   done
   ~~~~~~~~

   There are more than 7 tests but the larger ones may time out.

2. The benchmark with automatically abduced predicates (Sec. 5.2) is in the tar-file
   ``benchmarks/slsat/cctests.tgz``.  To run the benchmark unpack this archive and iterate the
   checker over the files.  For instance the following command will do this.

   ~~~~~~~~
   for F in <path where the archive was unpacked>/*.defs; do
    	./sl_satcheck.native -s -D $F;
   done
   ~~~~~~~~

   Bear in mind that there are almost 50k files in the test suite so unpacking and
   running the benchmark will take a long time.

3. To generate and run the randomly generated benchmarks (Sec. 5.3), first
   make sure Perl and the Perl libraries ``Math::Random`` and ``String::Urandom`` are
   installed. Then, do

	$ cd benchmarks/slsat/syn ; rm *.dat ; make

