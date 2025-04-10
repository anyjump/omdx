(lang dune 3.17)
(name omdx)

(generate_opam_files true)

(source
 (github anyjump/omdx))

(authors "Ed <ed@anyjump.com>")

(maintainers "Ed <ed@anyjump.com>")

(license LICENSE)

(documentation https://url/to/documentation)

; (implicit_transitive_deps true)

(package
 (name omdx)
 (allow_empty)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (ocaml (>= 5.0.0))
  uucp
  uutf
  uunf
  ppx_expect)
 (tags
  ("add topics" "to describe" omdx)))
