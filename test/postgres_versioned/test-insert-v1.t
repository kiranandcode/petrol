  $ ../test_postgres_versioned.exe createdb ver_test_insert_v1
  $ ../test_postgres_versioned.exe ver_test_insert_v1 v1 init
  $ ../test_postgres_versioned.exe ver_test_insert_v1 v1 add 0 http://ocaml.org
  $ ../test_postgres_versioned.exe ver_test_insert_v1 v1 list
  [0] - bookmark "": http://ocaml.org
  	age: -1
  	tags: 
  $ ../test_postgres_versioned.exe dropdb ver_test_insert_v1
