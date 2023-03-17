  $ ../test_postgres_versioned.exe createdb ver_test_insert_v1_extra
  $ ../test_postgres_versioned.exe ver_test_insert_v1_extra v1 init
  $ ../test_postgres_versioned.exe ver_test_insert_v1_extra v1 add 0 http://ocaml.org 10 john tag1 tag2
  $ ../test_postgres_versioned.exe ver_test_insert_v1_extra v1 list
  [0] - bookmark "": http://ocaml.org
  	age: -1
  	tags: 
  $ ../test_postgres_versioned.exe dropdb ver_test_insert_v1_extra
