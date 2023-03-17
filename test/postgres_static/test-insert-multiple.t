  $ ../test_postgres_static.exe createdb testing_insert_multiple
  $ ../test_postgres_static.exe testing_insert_multiple init
  $ ../test_postgres_static.exe testing_insert_multiple add john 30
  $ ../test_postgres_static.exe testing_insert_multiple add sally 15
  $ ../test_postgres_static.exe testing_insert_multiple add barry 14
  $ ../test_postgres_static.exe testing_insert_multiple add bob 20
  $ ../test_postgres_static.exe testing_insert_multiple add harry 10
  $ ../test_postgres_static.exe testing_insert_multiple list
  [0] - name: john; age: 30
  [1] - name: sally; age: 15
  [2] - name: barry; age: 14
  [3] - name: bob; age: 20
  [4] - name: harry; age: 10
  $ ../test_postgres_static.exe dropdb testing_insert_multiple
