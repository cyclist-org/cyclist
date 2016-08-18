---
layout: index
---
[TABLEAUX15]: http://dx.doi.org/10.1007/978-3-319-24312-2_20
{:target="_blank"}
[binary release]: https://github.com/ngorogiannis/cyclist/releases/tag/TABLEAUX15
{:target="_blank"}

``sl_disprove``
====================================================
``sl_disprove`` is a heuristic procedure for *disproving* SL entailments.
The tool and the theory behind it is described in the [[TABLEAUX15]] paper.

Compiling
----------------------------------------------------
If you are simply looking for the latest version of ``sl_disprove``, have a look at [Installation](installation).

If you want to try out the original version underpinning the [[TABLEAUX15]] paper, then you can
download an archive including x86_64 binaries and the original benchmark suite at the 
[TABLEAUX15 GitHub release](https://github.com/ngorogiannis/cyclist/releases/tag/TABLEAUX15).

Running

    $ sl_disprove.native 

will print out instructions for use.

Example
-------------------
Is a linked-list segment from ``x`` to ``y`` also a linked-list segment from ``y`` to ``x``?

    $ ./sl_disprove.native -S "ls(x,y) |- ls(y,x)"
    INVALID: ls^1(x, y) |- ls^2(y, x)

No, because the chain of pointers from ``x`` to ``y`` isn't (in general) a chain from ``y`` to ``x``.

Benchmarks in [TABLEAUX15]
----------------------------------------------------
There are three classes of benchmarks described in the paper.  The classes
SLL and UDP are from the [SL-COMP14](https://www.irif.univ-paris-diderot.fr/~sighirea/slcomp14/) 
competition and can be obtained from the
[SL-COMP14 GitHub repository](https://github.com/mihasighi/smtcomp14-sl).

The third class, LEM, is included in the *Cyclist* repository, under ``benchmarks/sl_disproof``.

The definitions are in ``all.defs`` and the sequents in ``seqs``.  The ``invbench.sh``
script will execute the LEM benchmark.