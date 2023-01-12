  $ ../test_versioned_bookmark.exe v2 test.db init
  $ ../test_versioned_bookmark.exe v2 test.db add 0 http://ocaml.org 100 search 1000 tag1 tag2 tag3 tag4
  $ ../test_versioned_bookmark.exe v2 test.db list
  [0] - bookmark "search": http://ocaml.org
  	age: 100
  	tags: 1000,tag1,tag2,tag3,tag4
  $ ../test_versioned_bookmark.exe v2 test.db clear-tags 0
  $ ../test_versioned_bookmark.exe v2 test.db list
  [0] - bookmark "search": http://ocaml.org
  	age: 100
  	tags: 
  $ ../test_versioned_bookmark.exe v2 test.db add-tag 0 random
  $ ../test_versioned_bookmark.exe v2 test.db list
  [0] - bookmark "search": http://ocaml.org
  	age: 100
  	tags: random
