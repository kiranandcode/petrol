  $ ../test_versioned_bookmark.exe v1 test.db init
  $ ../test_versioned_bookmark.exe v1 test.db add 0 http://ocaml.org
  $ ../test_versioned_bookmark.exe v1 test.db add 1 http://discuss.ocaml.org
  $ ../test_versioned_bookmark.exe v1 test.db add 2 http://github.com
  $ ../test_versioned_bookmark.exe v1 test.db add 3 http://fsf.org
  $ ../test_versioned_bookmark.exe v2 test.db list
  [0] - bookmark "unnamed": http://ocaml.org
  	age: 1000
  	tags: 
  [1] - bookmark "unnamed": http://discuss.ocaml.org
  	age: 1000
  	tags: 
  [2] - bookmark "unnamed": http://github.com
  	age: 1000
  	tags: 
  [3] - bookmark "unnamed": http://fsf.org
  	age: 1000
  	tags: 

