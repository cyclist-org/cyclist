(lang dune 2.2)

(name cyclist)

(authors
  "Nikos Gorogiannis <nikos.gorogiannis+www@gmail.com>"
  "Reuben Rowe <reuben.rowe@rhul.ac.uk>"
  "Gadi Tellez Espinosa"
  "Adham Jabarin"
  "Matan Shaked")

(maintainers
  "Nikos Gorogiannis <nikos.gorogiannis+www@gmail.com>"
  "Reuben Rowe <reuben.rowe@rhul.ac.uk>")

(license BSD-3-Clause)

(source (github cyclist-org/cyclist))

(homepage http://www.cyclist-prover.org/)

(package
 (name cyclist)
 (synopsis "Cyclist is a framework for building cyclic theorem provers")
 (depends
  (ocaml (>= 4.08.0))
  (stdlib-shims :build)
  (dune-configurator :build)
  (mparser-re (>= 1.3))
  (hashcons (>= 1.0.1))
  (hashset (>= 1.0.0))
  (ocamlgraph (>= 1.8.7))))

(formatting disabled)

(generate_opam_files true)