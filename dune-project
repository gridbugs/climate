(lang dune 3.0)

(generate_opam_files true)
(name climate)

(source (github gridbugs/climate))
(license MIT)
(authors "Stephen Sherratt")
(maintainers "https://github.com/gridbugs/climate/issues")

(cram enable)

(package
 (name climate)
 (synopsis "Declarative command-line parser for OCaml")
 (depends
   ocaml))

(package
 (name climate_cmdliner)
 (synopsis "Implements a subset of cmdliner's interface using climate")
 (depends
   ocaml
   (climate (= :version))))

(package
 (name climate_tests)
 (synopsis "Tests for climate packages")
 (depends
   ocaml
   climate
   ppx_inline_test
   ppx_expect))
