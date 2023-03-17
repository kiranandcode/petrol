  $ ../test_postgres_static.exe createdb testing_delete
  $ ../test_postgres_static.exe testing_delete init
  $ ../test_postgres_static.exe testing_delete add john 30
  $ ../test_postgres_static.exe testing_delete add sally 15
  $ ../test_postgres_static.exe testing_delete add barry 14
  $ ../test_postgres_static.exe testing_delete add bob 20
  $ ../test_postgres_static.exe testing_delete add harry 10
  $ ../test_postgres_static.exe testing_delete delete sally
  $ ../test_postgres_static.exe testing_delete list
  [0] - name: john; age: 30
  [1] - name: barry; age: 14
  [2] - name: bob; age: 20
  [3] - name: harry; age: 10
  $ ../test_postgres_static.exe dropdb testing_delete
