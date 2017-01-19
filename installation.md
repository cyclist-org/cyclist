---
layout: index
---

Compiling Cyclist from scratch
=======================================

*Cyclist* is hosted on the [Cyclist GitHub repository](https://github.com/ngorogiannis/cyclist). 

To compile things from scratch you will need the following (versions indicated are not minimums, ones we have tested):

* OCaml (tested with 4.02) [Debian packages: ocaml-nox, ocaml-native-compilers]

* g++ and headers (tested with 6.2) [Debian packages: g++, libstdc++-6.2-dev]

* Melt (tested with 1.4.0) [Debian package: ocaml-melt]

* Spot (tested with 2.2.1) [sources at https://spot.lrde.epita.fr ]
  &mdash; N.B. do not use versions 2.0.x and 2.1.x, since they contain a bug which can cause Cyclist to fail to find proofs.

* PCRE bindings for OCaml (tested with 7.2.3) [Debian package: libpcre-ocaml-dev]

* Ocamlgraph (tested with 1.8.7) [Debian package: libocamlgraph-ocaml-dev]

Then you need to clone the repository and run ``make``.

    $ git clone https://github.com/ngorogiannis/cyclist.git cyclist
    $ cd cyclist
    $ make

In non-standard installations you may need to change the paths in the top
of ``myocamlbuild.ml``.


