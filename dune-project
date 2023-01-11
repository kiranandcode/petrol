(lang dune 3.3)

(name petrol)

(generate_opam_files true)
(cram enable)

(authors "gopiandcode")

(maintainers "gopiandcode")

(license LGPL-3.0-linking-exception)

(homepage "https://github.com/gopiandcode/petrol")
(bug_reports "https://github.com/gopiandcode/petrol/issues")

(package
 (name petrol)
 (synopsis "Petrol's an OCaml SQL API made to go FAST.")
 (description "Petrol is an typed embedding of SQL into OCaml integrated with Caqti and support for versioning.")
 (depends
   ocaml dune caqti caqti-lwt caqti-driver-sqlite3)
 (tags
  (sql)))
