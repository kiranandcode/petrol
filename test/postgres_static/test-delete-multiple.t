  $ ../test_postgres_static.exe createdb testing_delete_multiple
  $ ../test_postgres_static.exe testing_delete_multiple init
  $ ../test_postgres_static.exe testing_delete_multiple add john 30
  $ ../test_postgres_static.exe testing_delete_multiple add sally 15
  $ ../test_postgres_static.exe testing_delete_multiple add barry 14
  $ ../test_postgres_static.exe testing_delete_multiple add bob 20
  $ ../test_postgres_static.exe testing_delete_multiple add harry 10
  $ ../test_postgres_static.exe testing_delete_multiple delete sally
  $ ../test_postgres_static.exe testing_delete_multiple delete barry
  $ ../test_postgres_static.exe testing_delete_multiple delete bob
  $ ../test_postgres_static.exe testing_delete_multiple list
  [0] - name: john; age: 30
  [1] - name: harry; age: 10
  $ ../test_postgres_static.exe dropdb testing_delete_multiple
