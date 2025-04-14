(lang dune 3.18)
(name omdx)
(generate_opam_files true)
(source
 (github anyjump/omdx))
(authors "Ed <ed@anyjump.com>")
(maintainers "Ed <ed@anyjump.com>")
(license MIT)
(documentation https://github.com/anyjump/omdx)
(implicit_transitive_deps true)

(package
 (name omdx)
 (allow_empty)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (ocaml (>= 5.3.0))
  uucp
  uutf
  uunf
  (ppx_expect :with-test))
 (tags
  ("add topics" "to describe" omdx)))

(package
 (name omdx-ppx)
 (allow_empty)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (ocaml (>= 5.3.0))
  (ppxlib (<= 0.36.0))
  omdx 
  (server-reason-react :with-test)
  (ppx_expect :with-test))
 (tags
  ("add topics" "to describe" omdx)))
