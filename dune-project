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
 (depends ocaml dune)
 (tags
  (image gif bitmap)))
