  $ ../test_postgres_static.exe createdb testing_insert_duplicate
  $ ../test_postgres_static.exe testing_insert_duplicate init
  $ ../test_postgres_static.exe testing_insert_duplicate add john 30
  $ ../test_postgres_static.exe testing_insert_duplicate add sally 15
  $ ../test_postgres_static.exe testing_insert_duplicate add barry 14
  $ ../test_postgres_static.exe testing_insert_duplicate add bob 20
  $ ../test_postgres_static.exe testing_insert_duplicate add harry 10
  $ ../test_postgres_static.exe testing_insert_duplicate add harry 10
  Fatal error: exception Request to <postgresql://localhost:5432/testing_insert_duplicate> failed: ERROR:  duplicate key value violates unique constraint "unique_names"
  DETAIL:  Key (name)=(harry) already exists.
   Query: "INSERT INTO person (name, age) VALUES ($1, $2)".
  [2]
  $ ../test_postgres_static.exe dropdb testing_insert_duplicate
