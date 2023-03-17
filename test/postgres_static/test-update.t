  $ ../test_postgres_static.exe createdb testing_update
  $ ../test_postgres_static.exe testing_update init
  $ ../test_postgres_static.exe testing_update add john 30
  $ ../test_postgres_static.exe testing_update add sally 15
  $ ../test_postgres_static.exe testing_update add barry 14
  $ ../test_postgres_static.exe testing_update add bob 20
  $ ../test_postgres_static.exe testing_update add harry 10
  $ ../test_postgres_static.exe testing_update update harry 15
  $ ../test_postgres_static.exe testing_update list
  [0] - name: john; age: 30
  [1] - name: sally; age: 15
  [2] - name: barry; age: 14
  [3] - name: bob; age: 20
  [4] - name: harry; age: 15
  $ ../test_postgres_static.exe dropdb testing_update
