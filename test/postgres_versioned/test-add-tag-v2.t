  $ ../test_postgres_versioned.exe createdb ver_test_add_tag_v2
  $ ../test_postgres_versioned.exe ver_test_add_tag_v2 v2 init
  $ ../test_postgres_versioned.exe ver_test_add_tag_v2 v2 add 0 http://ocaml.org 100 search
  $ ../test_postgres_versioned.exe ver_test_add_tag_v2 v2 add-tag 0 john
  $ ../test_postgres_versioned.exe ver_test_add_tag_v2 v2 list
  [0] - bookmark "search": http://ocaml.org
  	age: 100
  	tags: john
  $ ../test_postgres_versioned.exe dropdb ver_test_add_tag_v2
