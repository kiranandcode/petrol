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
  $ ../test_static_person.exe add test.db harry 10
  Fatal error: exception Response from <sqlite3:///test.db> failed: UNIQUE constraint failed: person.name. Query: "INSERT INTO person (name, age) VALUES (?1, ?2)\nRETURNING person.id".
  [2]
