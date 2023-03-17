# Petrol

Petrol is a Free software library that provides a high-level OCaml API
for interacting with SQL databases. The aim of this interface is to
provide a type-safe API to allow developers to define their SQL tables
and queries directly in OCaml, thereby avoiding the impedence mismatch
and fragility that comes with having to directly write SQL code, as is
typical in a normal Caqti-based project.

```ocaml
open Petrol
open Petrol.Sqlite3

(* define a new schema *)
let schema = StaticSchema.init ()

(* declare a table *)
let example_table, Expr.[name; age] =
    StaticSchema.declare_table schema ~name:"example"
    Schema.[
        field "name" ~ty:Type.text;
        field "age" ~ty:Type.int
    ]
```

Petrol's DSL allows you to express complex SQL queries as simple OCaml
function compositions:

```ocaml
open Petrol.Sqlite3

(* create a query *)
let insert_person ~name:n ~age:a db =
    Query.insert ~table:example_table
        ~values:Expr.[
            name := s n;
            age := i a
         ]
    |> Request.make_zero
    |> Petrol.exec db
```


See the rest of the documentation at [here](https://gopiandcode.github.io/petrol/petrol/index.html).
