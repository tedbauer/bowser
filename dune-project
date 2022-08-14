(lang dune 3.4)

(name bowser)

(generate_opam_files true)

(package
 (name bowser)
 (depends base graphics stdio menhir ocamlformat ppx_deriving ppx_sexp_conv)
 (synopsis "Toy browser engine"))

(using menhir 2.0)
