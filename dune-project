(lang dune 3.0)

(generate_opam_files true)

(name climate)

(source
 (github gridbugs/climate))

(license MIT)

(authors "Stephen Sherratt <stephen@sherra.tt>")

(maintainers "Stephen Sherratt <stephen@sherra.tt>")

(cram enable)

(package
 (name climate)
 (synopsis "Declarative command-line parser for OCaml")
 (depends
  (ocaml
   (>= 4.14))))

(package
 (name climate_cmdliner)
 (synopsis "Implements a subset of cmdliner's interface using climate")
 (depends
  (ocaml
   (>= 4.14))
  (climate
   (= :version))))

(package
 (name climate_tests)
 (synopsis "Tests for climate packages")
 (depends
  (ocaml
   (>= 4.14))
  climate
  ppx_inline_test
  ppx_expect))
