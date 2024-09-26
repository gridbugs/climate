(lang dune 3.0)

(generate_opam_files true)

(cram enable)

(name climate)

(package
 (name climate)
 (depends ocaml dune))

(package
 (name climate_cmdliner)
 (depends ocaml dune climate))

(package
 (name climate_tests)
 (depends ocaml dune climate ppx_inline_test ppx_expect))
