  $ ../test_postgres_versioned.exe createdb ver_test_insert_v12
  $ ../test_postgres_versioned.exe ver_test_insert_v12 v1.2 init
  $ ../test_postgres_versioned.exe ver_test_insert_v12 v1.2 add 0 http://ocaml.org 100
  $ ../test_postgres_versioned.exe ver_test_insert_v12 v1.2 list
  [0] - bookmark "": http://ocaml.org
  	age: 100
  	tags: 
  $ ../test_postgres_versioned.exe dropdb ver_test_insert_v12
