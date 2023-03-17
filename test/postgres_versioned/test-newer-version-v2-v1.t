  $ ../test_postgres_versioned.exe createdb ver_test_newer_version_v2_v1
  $ ../test_postgres_versioned.exe ver_test_newer_version_v2_v1 v2 init
  $ ../test_postgres_versioned.exe ver_test_newer_version_v2_v1 v2 add 0 http://ocaml.org
  $ ../test_postgres_versioned.exe ver_test_newer_version_v2_v1 v2 add 1 http://discuss.ocaml.org
  $ ../test_postgres_versioned.exe ver_test_newer_version_v2_v1 v2 add 2 http://github.com
  $ ../test_postgres_versioned.exe ver_test_newer_version_v2_v1 v2 add 3 http://fsf.org
  $ ../test_postgres_versioned.exe ver_test_newer_version_v2_v1 v1 list
  Fatal error: exception Attempted to use a newer database than supported
  [2]

  $ ../test_postgres_versioned.exe dropdb ver_test_newer_version_v2_v1
