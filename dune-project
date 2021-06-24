(lang dune 2.2)

(name cyclist)

(authors 
  "Nikos Gorogiannis <nikos.gorogiannis+www@gmail.com>"
  "Reuben Rowe <reuben.rowe@rhul.ac.uk>"
  "Gadi Tellez Espinosa")

(maintainers 
  "Nikos Gorogiannis <nikos.gorogiannis+www@gmail.com>"
  "Reuben Rowe <reuben.rowe@rhul.ac.uk>")

(source (github ngorogiannis/cyclist))
(homepage http://www.cyclist-prover.org/)

(package
 (name cyclist)
 (depends
  (ocaml (and (>= 4.02.0) (< 4.11.0)))
  (mparser (and (>= 1.2) (< 1.2.3)))
  (hashcons (>= 1.0.1))
  (hashset (>= 1.0.0))
  (ocamlgraph (>= 1.8.7))))

(formatting disabled)

(generate_opam_files true)