  $ ../test_static_person.exe init test.db
  $ ../test_static_person.exe add test.db john 30
  - id: 1
  $ ../test_static_person.exe add test.db sally 9
  - id: 2
  $ ../test_static_person.exe add test.db barry 14
  - id: 3
  $ ../test_static_person.exe add test.db bob 20
  - id: 4
  $ ../test_static_person.exe add test.db harry 10
  - id: 5
  $ ../test_static_person.exe find-older-than test.db 10
  [0] - name: john; age: 30
  [1] - name: barry; age: 14
  [2] - name: bob; age: 20
