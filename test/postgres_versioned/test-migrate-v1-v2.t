  $ ../test_postgres_versioned.exe createdb ver_test_migrate_v1_v2
  $ ../test_postgres_versioned.exe ver_test_migrate_v1_v2 v1 init
  $ ../test_postgres_versioned.exe ver_test_migrate_v1_v2 v1 add 0 http://ocaml.org
  $ ../test_postgres_versioned.exe ver_test_migrate_v1_v2 v1 add 1 http://discuss.ocaml.org
  $ ../test_postgres_versioned.exe ver_test_migrate_v1_v2 v1 add 2 http://github.com
  $ ../test_postgres_versioned.exe ver_test_migrate_v1_v2 v1 add 3 http://fsf.org
  $ ../test_postgres_versioned.exe ver_test_migrate_v1_v2 v2 list
  [0] - bookmark "unnamed": http://ocaml.org
  	age: 1000
  	tags: 
  [1] - bookmark "unnamed": http://discuss.ocaml.org
  	age: 1000
  	tags: 
  [2] - bookmark "unnamed": http://github.com
  	age: 1000
  	tags: 
  [3] - bookmark "unnamed": http://fsf.org
  	age: 1000
  	tags: 

  $ ../test_postgres_versioned.exe dropdb ver_test_migrate_v1_v2
