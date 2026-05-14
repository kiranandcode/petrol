  $ ../test_postgres_static_pet.exe init test.db
  $ ../test_postgres_static_pet.exe add-dog test.db Pluto 8 sausage
  - id: 1
  $ ../test_postgres_static_pet.exe add-dog test.db Bella 2 cake 
  - id: 2
  $ ../test_postgres_static_pet.exe add-dog test.db Gandalf 15 hamburger 
  - id: 3
  $ ../test_postgres_static_pet.exe add-cat test.db Garfield 8 18
  - id: 4
  $ ../test_postgres_static_pet.exe add-cat test.db Elvis 16 20
  - id: 5
  $ ../test_postgres_static_pet.exe add-cat test.db Grumbles 4 17
  - id: 6
  $ ../test_postgres_static_pet.exe get-all test.db
  [0] - name: Pluto; age: 8; favourite_food: sausage
  [1] - name: Bella; age: 2; favourite_food: cake
  [2] - name: Gandalf; age: 15; favourite_food: hamburger
  [3] - name: Garfield; age: 8; sleeping_hours: 18
  [4] - name: Elvis; age: 16; sleeping_hours: 20
  [5] - name: Grumbles; age: 4; sleeping_hours: 17
  $ ../test_postgres_static_pet.exe get-older-than test.db 10
  [0] - name: Gandalf; age: 15; favourite_food: hamburger
  [1] - name: Elvis; age: 16; sleeping_hours: 20
