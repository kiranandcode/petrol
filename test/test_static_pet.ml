[@@@warning "-27-26"]
open Lwt_result.Syntax

let db = Petrol.StaticSchema.init ()

type pet =
  | Dog of { name: string; age: int; favourite_food: string }
  | Cat of { name: string; age: int; sleeping_hours: int }

module Pet = struct
  open Petrol
  open Petrol.Sqlite3
  let t, Expr.[ id; name; age ] =
    StaticSchema.declare_table db ~name:"pet"
      Schema.[
        field ~constraints:[primary_key ~auto_increment:true ()] "id" ~ty:Type.int;
        field "name" ~ty:Type.text;
        field "age" ~ty:Type.int;
      ]

  module Dog = struct
    let t, Expr.[ id; favourite_food ] =
      StaticSchema.declare_table db ~name:"dog"
        Schema.[
          field
            "id"
            ~ty:Type.int
            ~constraints:
              [ primary_key (); foreign_key ~table:t ~columns:[ id ] () ];
          field "favourite_food" ~ty:Type.text
        ]
  end

  module Cat = struct
    let t, Expr.[ id; sleeping_hours ] =
      StaticSchema.declare_table db ~name:"cat"
        Schema.[
          field
            "id"
            ~ty:Type.int
            ~constraints:
              [ primary_key (); foreign_key ~table:t ~columns:[ id ] () ];
          field "sleeping_hours" ~ty:Type.int
        ]

  end

  let insert (p: pet) db =
    match p with
    | Dog dog ->
        let* (id, ()) =
          Query.insert ~table:t ~values:Expr.[ name := s dog.name; age := i dog.age ]
          |> Query.returning Expr.[ id ]
          |> Request.make_one
          |> Petrol.find db
        in
        let* () =
          Query.insert
            ~table:Dog.t
            ~values:
              Expr.[ Dog.id := i id; Dog.favourite_food := s dog.favourite_food ]
          |> Request.make_zero
          |> Petrol.exec db
        in
        Lwt_result.return id
    | Cat cat ->
        let* (id, ()) =
          Query.insert ~table:t ~values:Expr.[ name := s cat.name; age := i cat.age ]
          |> Query.returning Expr.[ id ]
          |> Request.make_one
          |> Petrol.find db
        in
        let* () = Query.insert
            ~table:Cat.t
            ~values:
              Expr.[ Cat.id := i id; Cat.sleeping_hours := i cat.sleeping_hours ]
          |> Request.make_zero
          |> Petrol.exec db
        in
        Lwt_result.return id

  let get_all db =
    (* join with "pet" table *)
    let* dogs =
      Query.select ~from:Dog.t Expr.[ name; age; Dog.favourite_food ]
      |> Query.join t ~on:Expr.(id = Dog.id)
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map
           (List.map (fun (name, (age, (favourite_food, ()))) ->
              Dog { name; age; favourite_food }))
    in
    let* cats =
      Query.select ~from:Cat.t Expr.[ name; age; Cat.sleeping_hours ]
      |> Query.join t ~on:Expr.(id = Cat.id)
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map
           (List.map (fun (name, (age, (sleeping_hours, ()))) ->
              Cat { name; age; sleeping_hours }))
    in
    Lwt_result.return (dogs @ cats)

  let get_older_than db age_ =
    (* join with subquery named "old_pets" *)
    let old_pets, Expr.[ old_pet_id; old_pet_name; old_pet_age ] =
      Query.select ~from:t Expr.[ id; name; age ]
      |> Query.where Expr.(age > i age_)
      |> Query.as_ ~name:"old_pets"
    in
    let* old_dogs =
      Query.select ~from:Dog.t Expr.[ old_pet_name; old_pet_age; Dog.favourite_food ]
      |> Query.join old_pets ~on:Expr.(old_pet_id = Dog.id)
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map
         (List.map (fun (name, (age, (favourite_food, ()))) ->
            Dog { name; age; favourite_food }))
    in
    let* old_cats =
      Query.select ~from:Cat.t Expr.[ old_pet_name; old_pet_age; Cat.sleeping_hours ]
      |> Query.join old_pets ~on:Expr.(old_pet_id = Cat.id)
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map
         (List.map (fun (name, (age, (sleeping_hours, ()))) ->
            Cat { name; age; sleeping_hours }))
    in
    Lwt_result.return (old_dogs @ old_cats)
end

let print_pets pets =
  List.iteri
    (fun i pet ->
      match pet with
      | Dog {name; age; favourite_food} ->
        Printf.printf
          "[%i] - name: %s; age: %i; favourite_food: %s\n"
          i
          name
          age
          favourite_food;
      | Cat {name; age; sleeping_hours} ->
        Printf.printf
          "[%i] - name: %s; age: %i; sleeping_hours: %i\n"
          i
          name
          age
          sleeping_hours;)
    pets

let () =
  Test_utils.main begin fun conn -> function[@warning "-8"]
    | "init", _ ->
      let* _ = Petrol.StaticSchema.initialise db conn in
      Lwt.return_ok ()
    | "add-dog", [ name; age; favourite_food ] ->
      let dog =
        Dog {
          name;
          age = int_of_string age;
          favourite_food
        }
      in
      let* _ = Petrol.StaticSchema.initialise db conn in
      let* id = Pet.insert dog conn in
      print_endline ("- id: " ^ string_of_int id);
      Lwt.return_ok ()
    | "add-cat", [ name; age; sleeping_hours ] ->
      let cat =
        Cat {
          name;
          age = int_of_string age;
          sleeping_hours = int_of_string sleeping_hours
        }
      in
      let* _ = Petrol.StaticSchema.initialise db conn in
      let* id = Pet.insert cat conn in
      Printf.printf "- id: %i\n" id;
      Lwt.return_ok ()
    | "get-all", _ ->
      let* _ = Petrol.StaticSchema.initialise db conn in
      let* pets = Pet.get_all conn in
      print_pets pets;
      Lwt.return_ok ()
    | "get-older-than", [ age ] ->
      let* _ = Petrol.StaticSchema.initialise db conn in
      let* pets = Pet.get_older_than conn (int_of_string age) in
      print_pets pets;
      Lwt.return_ok ()
  end


