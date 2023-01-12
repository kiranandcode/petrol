  $ ../test_versioned_bookmark.exe v2 test.db init
  $ ../test_versioned_bookmark.exe v2 test.db add 0 http://ocaml.org
  $ ../test_versioned_bookmark.exe v2 test.db add 1 http://discuss.ocaml.org
  $ ../test_versioned_bookmark.exe v2 test.db add 2 http://github.com
  $ ../test_versioned_bookmark.exe v2 test.db add 3 http://fsf.org
  $ ../test_versioned_bookmark.exe v1 test.db list
  Fatal error: exception Attempted to use a newer database than supported
  [2]

