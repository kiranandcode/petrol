  $ ../test_postgres_static.exe createdb testing_find_missing
  $ ../test_postgres_static.exe testing_find_missing init
  $ ../test_postgres_static.exe testing_find_missing add john 30
  $ ../test_postgres_static.exe testing_find_missing add sally 15
  $ ../test_postgres_static.exe testing_find_missing add barry 14
  $ ../test_postgres_static.exe testing_find_missing add bob 20
  $ ../test_postgres_static.exe testing_find_missing add harry 10
  $ ../test_postgres_static.exe testing_find_missing find-by darren
  not found
  $ ../test_postgres_static.exe dropdb testing_find_missing
