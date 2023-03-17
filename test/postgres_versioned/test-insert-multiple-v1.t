  $ ../test_postgres_versioned.exe createdb ver_test_insert_multiple
  $ ../test_postgres_versioned.exe ver_test_insert_multiple v1 init
  $ ../test_postgres_versioned.exe ver_test_insert_multiple v1 add 0 http://ocaml.org
  $ ../test_postgres_versioned.exe ver_test_insert_multiple v1 add 1 http://discuss.ocaml.org
  $ ../test_postgres_versioned.exe ver_test_insert_multiple v1 add 2 http://github.com
  $ ../test_postgres_versioned.exe ver_test_insert_multiple v1 add 3 http://fsf.org
  $ ../test_postgres_versioned.exe ver_test_insert_multiple v1 list
  [0] - bookmark "": http://ocaml.org
  	age: -1
  	tags: 
  [1] - bookmark "": http://discuss.ocaml.org
  	age: -1
  	tags: 
  [2] - bookmark "": http://github.com
  	age: -1
  	tags: 
  [3] - bookmark "": http://fsf.org
  	age: -1
  	tags: 
  $ ../test_postgres_versioned.exe dropdb ver_test_insert_multiple
