  $ ../test_static_person.exe init test.db
  $ ../test_static_person.exe add test.db john 30
  $ ../test_static_person.exe add test.db sally 15
  $ ../test_static_person.exe add test.db barry 14
  $ ../test_static_person.exe add test.db bob 20
  $ ../test_static_person.exe add test.db harry 10
  $ ../test_static_person.exe add test.db harry 10
  Fatal error: exception Response from <sqlite3:///test.db> failed: UNIQUE constraint failed: person.name. Query: "INSERT INTO person (name, age) VALUES (?1, ?2)".
  [2]
