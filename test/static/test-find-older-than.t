  $ ../test_static_person.exe init test.db
  $ ../test_static_person.exe add test.db john 30
  $ ../test_static_person.exe add test.db sally 9
  $ ../test_static_person.exe add test.db barry 14
  $ ../test_static_person.exe add test.db bob 20
  $ ../test_static_person.exe add test.db harry 10
  $ ../test_static_person.exe find-older-than test.db 10
  [0] - name: john; age: 30
  [1] - name: barry; age: 14
  [2] - name: bob; age: 20
