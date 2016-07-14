---
layout: index
---

Satisfiability checker for separation logic with inductive definitions.
=======================================================================

THEORY:
-----------------------------------

The algorithm, its soundness and its complexity are described in the [CSL-LICS, 2014 paper](http://dx.doi.org/10.1145/2603088.2603091):

  *James Brotherston, Carsten Fuhs, Nikos Gorogiannis, and Juan Navarro Pérez.
  A decision procedure for satisfiability in separation logic with inductive
  predicates. To appear at CSL-LICS, 2014.*


QUICKSTART:
-----------------------------------
If you downloaded a tarball then the executables in the archive will have been
linked in such a way that they should work without any additional dependencies
on an x86_64 Linux system.

Running
  	`sl_satcheck.native -h`
will print out instructions for use.


RUNNING THE CSL-LICS14 TEST SUITE:
-------------------------------------
Assuming you have downloaded or built the binaries already:

1. A superset of the hand-written tests described in the paper (Sec. 5.1) are in
examples/sl.defs.  To check their satisfiability, run

	`./sl_satcheck.native`

The worst-case complexity example is in benchmarks/slsat/cc/succ-circuit*.defs.
To run this test iterate the checker over these files, i.e.,

~~~~~~~~
for F in benchmarks/cc/succ-circuit[1-7].defs ; do
	./slsat_check.native -s -D $F -t 2400;
done
~~~~~~~~

There are more than 7 tests but the larger ones may time out.

2. The benchmark with automatically abduced predicates (Sec. 5.2) is in the tar-file
benchmarks/slsat/cctests.tgz.  To run the benchmark unpack this archive and iterate the
checker over the files.  For instance the following command will do this.

~~~~~~~~
for F in <path where the archive was unpacked>/*.defs; do
	./slsat_check.native -s -D $F;
done
~~~~~~~~

Bear in mind that there are almost 50k files in the test suite so unpacking and
running the benchmark will take a long time.

3. To generate and run the randomly generated benchmarks (Sec. 5.3), first
make sure Perl and the Perl libraries Math::Random and String::Urandom are
installed. Then, do

  	`cd benchmarks/slsat/syn ; rm *.dat ; make`