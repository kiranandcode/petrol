  $ ../test_postgres_static.exe createdb testing_find_older_than
  $ ../test_postgres_static.exe testing_find_older_than init
  $ ../test_postgres_static.exe testing_find_older_than add john 30
  $ ../test_postgres_static.exe testing_find_older_than add sally 9
  $ ../test_postgres_static.exe testing_find_older_than add barry 14
  $ ../test_postgres_static.exe testing_find_older_than add bob 20
  $ ../test_postgres_static.exe testing_find_older_than add harry 10
  $ ../test_postgres_static.exe testing_find_older_than find-older-than 10
  [0] - name: john; age: 30
  [1] - name: barry; age: 14
  [2] - name: bob; age: 20
  $ ../test_postgres_static.exe dropdb testing_find_older_than
