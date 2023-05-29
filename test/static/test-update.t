  $ ../test_static_person.exe init test.db
  $ ../test_static_person.exe add test.db john 30
  - id: 1
  $ ../test_static_person.exe add test.db sally 15
  - id: 2
  $ ../test_static_person.exe add test.db barry 14
  - id: 3
  $ ../test_static_person.exe add test.db bob 20
  - id: 4
  $ ../test_static_person.exe add test.db harry 10
  - id: 5
  $ ../test_static_person.exe update test.db harry 15
  $ ../test_static_person.exe list test.db
  [0] - name: john; age: 30
  [1] - name: sally; age: 15
  [2] - name: barry; age: 14
  [3] - name: bob; age: 20
  [4] - name: harry; age: 15
