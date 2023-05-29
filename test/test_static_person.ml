[@@@warning "-27-26"]
open Lwt_result.Syntax

let db = Petrol.StaticSchema.init ()

module Person = struct

  type t = {name: string; age: int}

  module Sql = struct
    open Petrol
    open Petrol.Sqlite3
    let t, Expr.[pid;name;age] =
      StaticSchema.declare_table db ~name:"person"
        Schema.[
          field ~constraints:[primary_key ~auto_increment:true ()] "id" ~ty:Type.int;
          field ~constraints:[unique ~name:"unique_names" ()] "name" ~ty:Type.text;
          field "age" ~ty:Type.int;
        ]

    let insert {name=n;age=a} db =
      Query.insert ~table:t ~values:Expr.[name := s n; age := i a]
      |> Query.returning Expr.[pid]
      |> Request.make_one
      |> Petrol.find db

    let delete ~name:nme db =
      Query.delete ~from:t
      |> Query.where Expr.(name = s nme)
      |> Request.make_zero
      |> Petrol.exec db

    let update_age ~name:nm a db =
      Query.update ~table:t ~set:Expr.[age := i a]
      |> Query.where Expr.(name = s nm)
      |> Request.make_zero
      |> Petrol.exec db

    let all db =
      Query.select Expr.[name;age] ~from:t
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map (List.map (fun (name, (age, ())) -> {name;age}))

    let find_older ~than  db =
      Query.select Expr.[name;age] ~from:t
      |> Query.where Expr.(age > i than)
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map (List.map (fun (name, (age, ())) -> {name;age}))

    let find_by ~name:nm  db =
      Query.select Expr.[name;age] ~from:t
      |> Query.where Expr.(name = s nm)
      |> Request.make_zero_or_one
      |> Petrol.find_opt db
      |> Lwt_result.map (Option.map (fun (name, (age, ())) -> {name;age}))

  end

  let random () =
    let random_char () =
      Char.unsafe_chr
        Char.((code 'a') + Random.int (code 'z' - code 'a' + 1)) in
    let random_string n =
      String.init n (fun _ -> random_char ()) in
    {
      name=random_string 10;
      age=Random.int 100;
    }

end

let () =
  Test_utils.main begin fun conn -> function[@warning "-8"]
    | "init", _ ->
      let* _ = Petrol.StaticSchema.initialise db conn in
      Lwt.return_ok ()
    | "add", [name;age] ->
      let age = int_of_string age in
      let* _ = Petrol.StaticSchema.initialise db conn in
      let* (pid, ()) = Person.Sql.insert Person.{name;age} conn in
      print_endline ("- id: " ^ string_of_int pid);
      Lwt.return_ok ()
    |  "find-by", [name] ->
      let* _ = Petrol.StaticSchema.initialise db conn in
      let* result = Person.Sql.find_by ~name:name conn in
      begin match result with
      | None -> print_endline "not found"
      | Some Person.{name;age} -> print_endline ("name: " ^ name ^ "; age: " ^ string_of_int age)
      end;
      Lwt.return_ok ()
    |  "find-older-than", [age] ->
      let age = int_of_string age in
      let* _ = Petrol.StaticSchema.initialise db conn in
      let* result = Person.Sql.find_older ~than:age conn in
      List.iteri (fun ind Person.{name;age} ->
        print_endline ("[" ^ string_of_int ind ^ "] - name: " ^ name ^ "; age: " ^ string_of_int age)
      ) result;
      Lwt.return_ok ()
    |  "list", _ ->
      let* _ = Petrol.StaticSchema.initialise db conn in
      let* result = Person.Sql.all conn in
      List.iteri (fun ind Person.{name;age} ->
        print_endline ("[" ^ string_of_int ind ^ "] - name: " ^ name ^ "; age: " ^ string_of_int age)
      ) result;
      Lwt.return_ok ()
    |  "update", [name; age] ->
      let age = int_of_string age in
      let* _ = Petrol.StaticSchema.initialise db conn in
      let* () = Person.Sql.update_age ~name age conn in
      Lwt.return_ok ()
    |  "delete", [name] ->
      let* _ = Petrol.StaticSchema.initialise db conn in
      let* () = Person.Sql.delete ~name conn in
      Lwt.return_ok ()
    |  "insert-random", _ ->
      let fname = Sys.argv.(2) in
      let* conn = Caqti_lwt.connect (Uri.of_string ("sqlite3://:" ^ fname)) in
      let* _ = Petrol.StaticSchema.initialise db conn in
      let person = Person.random () in
      let* (pid, ()) = Person.Sql.insert person conn in
      print_endline ("- id: " ^ string_of_int pid);
      Lwt.return_ok ()
  end


