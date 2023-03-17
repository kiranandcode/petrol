  $ ../test_postgres_versioned.exe createdb ver_test_clear_tags_v2
  $ ../test_postgres_versioned.exe ver_test_clear_tags_v2 v2 init
  $ ../test_postgres_versioned.exe ver_test_clear_tags_v2 v2 add 0 http://ocaml.org 100 search 1000 tag1 tag2 tag3 tag4
  $ ../test_postgres_versioned.exe ver_test_clear_tags_v2 v2 list
  [0] - bookmark "search": http://ocaml.org
  	age: 100
  	tags: 1000,tag1,tag2,tag3,tag4
  $ ../test_postgres_versioned.exe ver_test_clear_tags_v2 v2 clear-tags 0
  $ ../test_postgres_versioned.exe ver_test_clear_tags_v2 v2 list
  [0] - bookmark "search": http://ocaml.org
  	age: 100
  	tags: 
  $ ../test_postgres_versioned.exe ver_test_clear_tags_v2 v2 add-tag 0 random
  $ ../test_postgres_versioned.exe ver_test_clear_tags_v2 v2 list
  [0] - bookmark "search": http://ocaml.org
  	age: 100
  	tags: random
  $ ../test_postgres_versioned.exe dropdb ver_test_clear_tags_v2
