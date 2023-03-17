  $ ../test_postgres_versioned.exe createdb ver_test_insert_v2
  $ ../test_postgres_versioned.exe ver_test_insert_v2 v2 init
  $ ../test_postgres_versioned.exe ver_test_insert_v2 v2 add 0 http://ocaml.org 100 search
  $ ../test_postgres_versioned.exe ver_test_insert_v2 v2 list
  [0] - bookmark "search": http://ocaml.org
  	age: 100
  	tags: 
  $ ../test_postgres_versioned.exe dropdb ver_test_insert_v2
