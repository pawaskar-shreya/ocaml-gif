(lang dune 3.14)

(name giflib)

(generate_opam_files true)

(source
 (github dudek/ocaml-gif))

(authors "Jan Dudek <jd@jandudek.com>")

(maintainers "Jan Dudek <jd@jandudek.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name giflib)
 (synopsis "Implementation of GIF image format in OCaml")
 (description "Implementation of GIF image format in OCaml.")
 (depends ocaml dune ounit2 camlp-streams zarith odoc (ocamlformat (>= 0.27.0)))
 (tags
  (image gif bitmap)))
