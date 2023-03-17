  $ ../test_postgres_static.exe createdb testing_insert
  $ ../test_postgres_static.exe testing_insert init 
  $ ../test_postgres_static.exe testing_insert add john 30
  $ ../test_postgres_static.exe testing_insert list
  [0] - name: john; age: 30
  $ ../test_postgres_static.exe dropdb testing_insert
