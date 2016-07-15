---
layout: index
---
[SAS'14 paper]: http://dx.doi.org/10.1007/978-3-319-10936-7_5
{:target="_blank"}

A heuristic procedure for disproving SL entailments.
====================================================

OVERVIEW:
----------------------------------------------------
The tool is described in the [SAS'14 paper].

  *J. Brotherston and N. Gorogiannis.
  Cyclic Abduction of Inductively Defined 
  Safety and Termination Preconditions*

QUICKSTART:
----------------------------------------------------
If you downloaded the Virtual Machine from the SAS website
you will find the executable `while_abduce.native` which
implements the algorithm described in the paper.  If you 
do not have such a binary, look at the file README.compiling.

Running the executable without options will produce some help 
text explaining its usage.

The command 

	make whl_abd-tests

will run the tests discussed in the paper.

*NB the Mutant tests were made available to us with permission
and are not included in this image.*

TEST SUITE:
----------------------------------------------------
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