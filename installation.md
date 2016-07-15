---
layout: index
---

Installation
=======================================

Releases
----------------------------------------

### Model checking for symbolic-heap separation logic with inductive predicates.

A compressed archive containing the test suite and Linux x64 binaries is below.

- - -

[cyclist.tar.bz2](https://github.com/ngorogiannis/cyclist/releases/download/POPL16/cyclist.tar.bz2)

- - -

[**Source code** (zip)](https://github.com/ngorogiannis/cyclist/archive/POPL16.zip)

- - -

[**Source code** (tar.gz)](https://github.com/ngorogiannis/cyclist/archive/POPL16.tar.gz)

- - -

### Disproving Inductive Entailments in Separation Logic via Base Pair Approximation.

A pre-compiled version of the code can be found below.

- - -

[cyclist.tar.bz2](https://github.com/ngorogiannis/cyclist/releases/download/TABLEAUX15/cyclist.tar.bz2)

- - -

[**Source code** (zip)](https://github.com/ngorogiannis/cyclist/archive/TABLEAUX15.zip)

- - -

[**Source code** (tar.gz)](https://github.com/ngorogiannis/cyclist/archive/TABLEAUX15.tar.gz)

- - -

### Cyclic Abduction of Inductively Defined Safety and Termination Preconditions.

A [link](http://cs.uni-muenster.de/sev/sas14/artifacts.php){:target="_blank"} to a VirtualBox virtual machine loaded with a pre-compiled version of the software and the test suite will soon be posted here.

- - -

[**Source code** (zip)](https://github.com/ngorogiannis/cyclist/archive/SAS14.zip)

- - -

[**Source code** (tar.gz)](https://github.com/ngorogiannis/cyclist/archive/SAS14.tar.gz)

- - -

### A satisfiability checker for separation logic with inductive predicates.

Binaries (for a Linux x64 system) and a test suite are included in the slsat.tar.gz file. After downloading, look at [Satisfiability Checker](/slsat_check) for instructions on how to run the tool and the test suite.

- - -

[cyclist.tar.bz2](https://github.com/ngorogiannis/cyclist/releases/download/CSL-LICS14/slsat.tar.gz)

- - -

[**Source code** (zip)](https://github.com/ngorogiannis/cyclist/archive/CSL-LICS14.zip)

- - -

[**Source code** (tar.gz)](https://github.com/ngorogiannis/cyclist/archive/CSL-LICS14.tar.gz)

- - -

Compiling Cyclist from scratch
----------------------------------------

``Cyclist`` is hosted on GitHub at https://github.com/ngorogiannis/cyclist . 

To compile things from scratch you will need the following:

* OCaml (tested with 4.01) [Debian packages: ocaml-nox, ocaml-native-compilers]

* g++ and headers (tested with 4.8) [Debian packages: g++, libstdc++-4.8-dev]

* Melt (tested with 1.4.0) [Debian package: ocaml-melt]

* Spot (tested with 2.0.2) [sources at https://spot.lrde.epita.fr ]

* PCRE bindings for OCaml (tested with 7.0.4) [Debian package: libpcre-ocaml-dev]

In non-standard installations you may need to change the paths in the top
of ``myocamlbuild.ml``.

Then, just run ``make`` and hopefully it should work :) 


