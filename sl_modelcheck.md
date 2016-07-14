---
layout: index
---

A model checker for SL with arbitrary inductive predicates.
===========================================================

The tool is described in the [POPL'16 paper](http://dx.doi.org/10.1145/2914770.2837621):

  *J. Brotherston, N. Gorogiannis, M. Kanovich, R. Rowe.
  Model Checking for Symbolic-Heap Separation Logic with Inductive Predicates*


QUICKSTART:
-----------------------------------
If you downloaded the binary release from GitHub a x64 binary
sl_modelcheck.native should be already present in the project directory.
If you do not have such a binary, look at the file README.compiling.

Running
  	`sl_modelcheck.native`
will print out instructions for use.

Our tool also includes a script that may be used to harvest models from programs
at runtime using the Archer extension that enables python scripting of GDB. Our
harvester script harvest_heap.py may be found in the utils/ and
utils/gdb_harvester subdirectories.

Running the script without options will produce some help text explaining its
usage.
The script expects as its first argument the path to an executable file
from which models are to harvested. It then expects subsequent argument triples
which are to consist of:
1. a breakpoint description;
2. a comma-separated list
of variables from which to start walking the heap;
3. a path where the files containing the harvested models are to saved.

TEST SUITE DESCRIPTION:
-----------------------

The benchmarks/slmc/ subdirectory contains all the necessary resources for running the benchmarks
described in the POPL'16 paper. Below this, there are two further sub-
directories. The models/ subdirectory contains *.mdl files, each of which
describes a model which can be parsed by the model checker. The programs/ sub-
directory contains the source code of the 7 test programs from which models
were harvested at runtime, along with binary versions of these programs,
compiled using gcc 4.2.2 on Ubuntu Linux 12.04LTS. These programs have been
taken from the test suite of the Verifast tool; they have been modified slightly
in order to successfully compile them and for our tool to be able to harvest
an appropriate selection of runtime models.

This directory contains three types of files:

*.defs
: these contain inductive predicate definitions against which our tool
checks the models harvested from the appropriate program.

*.tst
: these files contains descriptors for the benchmark tests to be run;
the descriptors consist of a filename prefix for selecting
appropriate *.mdl files, and a separation logic assertion (formula)
against which to test the models.

*.bps
: these files contain descriptors that specify how to harvest runtime
models from the test programs. For details of the harvesting tool,
see below.

Within this directory, running the make command with the various targets 
discussed below will execute the tests referred to in the paper. In the
following, {TEST} will stand for one of the tokens in the set { aplas-stack, composite, iter, lcset, queue, schorr-waite }

[make quick-tests]
: will run a small subset of the tests in the benchmark suite
as a quick representative demonstration of the tool, and display the results
(with info of running times) to standard output.

[make {TEST}-tests]
: will run all the benchmark tests associated with the {TEST}
program and display the results (with info of running times) to standard
output.

[make {TEST}-ptime-tests]
: will run the benchmark tests associated with the
{TEST} programs that fall within the PTIME fragment, using the PTIME
algorithm and display the results (with info of running times) to standard
output. The targets [composite-ptime-tests] and [lcset-ptime-tests] are not
defined, since none of the benchmark tests for these programs fall within
the PTIME fragment.

[make spatial-true-tests]
: will model check a substantial selection of the
models against encodings of the spatial truth constant T*, which every
model satisfies. The results (with info of running times) are displayed
to standard output.

[make binary-counter-tests]
: will run the model checker against predicate
definitions encoding 1-bit, 2-bit and 3-bit binary counters, which fall
within the EXPTIME-complete general fragment. The results (with info of
running times) are displayed to standard output.

[make all-tests]
: will call each of the {TEST}-tests, spatial-true-tests and 
binary-counter-tests targets in turn.

[make all-ptime-tests]
: will call each of the (defined) ptime test targets in turn.

[make {Target}-stats]
: will call the appropriate [{Target}-tests] target (as
described above) and pipe the output through a script that extracts the
running time statics for each benchmark test and saves it in a .csv file in
the stats/ subdirectory. The statistics for each set of tests described by
one of the *.tst files (see above) is saved in a correspondingly named .csv
file, which also contains a timestamp; thus tests may be run multiple times
and the running time statistics easily collected for each run individually.

N.B. Running the entire set of benchmark tests (e.g. using the [all-tests]
target) will take a long time. The [spatial-true-tests] are the longest single
set of benchmark tests, followed by [lcset-tests] and [schorr-waite-tests]


RUNTIME HARVESTING OF MODELS:
-----------------------------

The makefile in this directory contains targets for running the harvester tool 
on the benchmark suite test programs.

[make {TEST}-models]
: will run the model harvester over the {TEST} program using
the descriptors found in the appropriate *.bps files. The resulting .mdl
files will be placed in the models/ subdirectory. The script does not 
overwrite any .mdl files it finds, but where files with the same filename 
prefix exist it will instead append a new numerical suffix to the newly 
generated files.

Note that when running the tests using the makefile [*-tests] targets the
tests specified by the .tst configuration files will be run over ALL the
.mdl files in the models/ subdirectory. Therefore running the tests AFTER
generating model files will cause the tests to be run over these new models
as well.

[make clean-{TEST}]
: will remove all of the model files within the models/ sub-
directory whose filenames are prefixed with {TEST}.

*N.B. Running the model-harvesting targets, i.e. [make {TEST}-stats] with {TEST}
in { aplas-stack, composite, iter, schorr-waite }, should result in the same set
of model files each time, since these are deterministic programs. However the 
lcset and queue programs are concurrent and so running the model-harvesting 
target for these programs will, in general, produce different sets of model 
files each time. The Cyclist source tree contains the set of .mdl files that 
were used to produce the benchmark results in the POPL'16 paper.*