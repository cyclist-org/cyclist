---
layout: index
---

Compiling Cyclist from scratch
=======================================

*Cyclist* is hosted on the [Cyclist GitHub repository](https://github.com/ngorogiannis/cyclist).

To compile things from scratch you will need the following.

* OCaml (tested with 4.02) [Debian packages: ocaml-nox, ocaml-native-compilers].

* g++ and headers (tested with 6.2) [Debian packages: g++, libstdc++-6.2-dev].

* Spot (version >= 2.4)
  [available at [https://spot.lrde.epita.fr](https://spot.lrde.epita.fr) ]
  accessible via pkg-config.

* PCRE bindings for OCaml (tested with 7.2.3)
  [Debian package: libpcre-ocaml-dev].

* The [dune](https://github.com/ocaml/dune) ocaml build system (version >= 2.2).

* Cyclist depends on the following OCaml packages.

  * hashset
  * hashcons
  * mparser (and it subpackage mparser.pcre)
  * ocamlgraph (tested with 1.8.7) [Debian package: libocamlgraph-ocaml-dev]

Then you need to clone the repository and run ``dune build``.

    git clone https://github.com/ngorogiannis/cyclist.git cyclist
    cd cyclist
    dune build
