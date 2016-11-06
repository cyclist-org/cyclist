---
layout: index
---

Compiling Cyclist from scratch
=======================================

*Cyclist* is hosted on the [Cyclist GitHub repository](https://github.com/ngorogiannis/cyclist). 

To compile things from scratch you will need the following:

* OCaml (tested with 4.01) [Debian packages: ocaml-nox, ocaml-native-compilers]

* g++ and headers (tested with 4.8) [Debian packages: g++, libstdc++-4.8-dev]

* Melt (tested with 1.4.0) [Debian package: ocaml-melt]

* Spot (tested with 2.0.2) [sources at https://spot.lrde.epita.fr ]

* PCRE bindings for OCaml (tested with 7.0.4) [Debian package: libpcre-ocaml-dev]

* Ocamlgraph (tested with 1.8.6) [Debian package: libocamlgraph-ocaml-dev]

Then you need to clone the repository and run ``make``.

    $ git clone https://github.com/ngorogiannis/cyclist.git cyclist
    $ cd cyclist
    $ make

In non-standard installations you may need to change the paths in the top
of ``myocamlbuild.ml``.


