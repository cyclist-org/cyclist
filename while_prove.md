---
layout: index
---
[Installation]: /installation
[APLAS12]: http://dx.doi.org/10.1007/978-3-642-35182-2_25
[SAS'14 paper]: http://dx.doi.org/10.1007/978-3-319-10936-7_5
{:target="_blank"}
[Virtual Machine]: https://github.com/ngorogiannis/cyclist/releases/tag/SAS14
{:target="_blank"}

``while_prove``
=============================================================
``while_prove`` is a termination and/or safety prover for simple while-programs 
specified by preconditions in SL. Postconditions are not used, so the prover does not
check for memory leaks or producing a specific heap after execution.

The ``while_prove`` tool is a significantly changed and updated version of the 
``goto_prove`` tool presented in the [[APLAS12]].  The ``goto_prove`` tool is deprecated
and unmaintained; if you are interested in seeing the source code or running it see below.

Compiling
----------------------------------------------------
If you are simply looking for the latest version of ``while_prove``, have a look at [Installation].

The original version of ``goto_prove`` in [[APLAS12]] is not stored on GitHub.  To try the closest possible, 
you can checkout a very early commit.

    git clone https://github.com/ngorogiannis/cyclist.git cyclist
    cd cyclist
    git checkout b289f39a6a46b4f57da3585aa7fd71f3ca8601fc

Then consult the README included, which contains building instructions
relevant to that version.

Running ``while_prove.native`` without options will produce some help text explaining its usage.

Example
--------------
Here is a program for use with ``while_prove``.

    fields: next;
    precondition: ls(x,nil); 
    while x!=nil do
      x := x.next
    od

The first line declares that heap cells are records with one field, called ``next`` (this program is for use with linked lists).

The second line states the precondition of the program.

The remainder is code in a while-language with if/while control structures and assignment/dereference.

Let's verify that this program *terminates safely* (i.e., without committing memory faults).

    $ ./while_prove.native -T -P benchmarks/whl/01-ls.wl -p
    0: ls^1(x, nil) |- 0: while x!=nil do 1: ... od (While) [1, 2]
      1: nil!=x * ls^1(x, nil) |- 1: x := x.next; 0: ... (ls L.Unf.) [3, 4]
        3: nil=x * nil!=x |- 1: x := x.next; 0: ... (Ex Falso)
        4: nil!=x * x->y' * ls^1(y', nil) |- 1: x := x.next; 0: ... (Load/Simplify) [5]
          5: nil!=x' * x'->x * ls^1(x, nil) |- 0: while x!=nil do 1: ... od (Frame) [6]
            6: ls^1(x, nil) |- 0: while x!=nil do 1: ... od (Backl) [0] <pre={(1, 1)}>
      2: nil=x * ls^1(x, nil) |-  (Empty)

The output includes a *cyclic* proof of termination 
(effectively an argument that the linked list provided as input is consumed as the program runs).

Benchmarks in [[APLAS12]]
----------------------------------------------------
The benchmarks in [[APLAS12]] were for the ``goto_prove`` tool which is no longer maintained.
Translated versions of these benchmarks are included in the *Cyclist* repository under
``benchmarks/whl``.

To run these benchmarks do

    $ make whl-tests

To obtain more information from the tests, the shell variable
``TST_OPTS`` will add command-line arguments to ``while_prove``.
For example, the command

    TST_OPTS=-s make whl-tests

will produce statistics for each test, including runtimes.
