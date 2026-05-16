  $ ../test_postgres_static_person.exe createdb testing_insert
  $ ../test_postgres_static_person.exe testing_insert init 
  $ ../test_postgres_static_person.exe testing_insert add john 30
  $ ../test_postgres_static_person.exe testing_insert list
  [0] - name: john; age: 30
  $ ../test_postgres_static_person.exe dropdb testing_insert
