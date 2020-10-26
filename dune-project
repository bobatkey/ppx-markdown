(lang dune 2.0)
(generate_opam_files true)

(name ppx_markdown)
(version 0.0.1)

(authors "Robert Atkey")
(license MIT)
(maintainers "bob.atkey@gmail.com")
(source (github bobatkey/ppx-markdown))

(package
 (name ppx_markdown)
 (synopsis "Syntax extension for inline Markdown")
 (description "Blah.")
 (depends
  (dune (>= 2.0))
  (ocaml (>= 4.04.0))
  (ppxlib (>= 0.15.0))

  (ounit :with-test)))
