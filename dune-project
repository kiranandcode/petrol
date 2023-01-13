(lang dune 3.3)

(name petrol)

(generate_opam_files true)
(cram enable)

(authors "gopiandcode")

(maintainers "gopiandcode")

(license LGPL-3.0-or-later)

(homepage "https://github.com/gopiandcode/petrol")
(bug_reports "https://github.com/gopiandcode/petrol/issues")

(package
 (name petrol)
 (synopsis "Petrol's an OCaml SQL API made to go FAST")
 (description "Petrol is an typed embedding of SQL into OCaml integrated with Caqti and support for versioning.")
 (depends
  (ocaml (>= 4.12))
  (dune (>= 3.3))
  (lwt (>= 5.5))
  (caqti (>= 1.8.0))
  (caqti-lwt (>= 1.8.0))
  (caqti-driver-sqlite3 (>= 1.8.0))
 )
 (tags
  (sql)))
