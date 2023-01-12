module Type = Type
module Schema = Schema
module Expr = Expr
module Query = Query
module Request = Request

type table_name = Types.table_name

let _result_all : ('a, 'e) result list -> ('a list, 'e) result =
  fun ls ->
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok (h) :: t -> loop (h :: acc) t
    | Error err :: _ -> Error err in
  loop [] ls

let result_all_unit : (unit, 'e) result list -> (unit, 'e) result =
  fun ls ->
  let rec loop = function
    | [] -> Ok ()
    | Ok () :: t -> loop t
    | Error err :: _ -> Error err in
  loop ls


let rec drop_while ~f ls =
  match ls with
  | [] -> []
  | h :: t when f h -> drop_while ~f t
  | ls -> ls

let exec : (module Caqti_lwt.CONNECTION) ->
  (unit,[< `Zero ]) Request.t -> (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t =
  fun (module DB: Caqti_lwt.CONNECTION) ((MkCaqti (inps,req),wrapp_value): (unit,_) Request.t) ->
  let data = Request.unwrap (inps,wrapp_value) in
  DB.exec req data

let find : 'a . (module Caqti_lwt.CONNECTION) ->
  ('a,[< `One ]) Request.t -> ('a, [> Caqti_error.call_or_retrieve ]) result Lwt.t =
  fun (module DB: Caqti_lwt.CONNECTION) (type a) ((MkCaqti (inps,req),wrapp_value): (a,_) Request.t) ->
  let data = Request.unwrap (inps,wrapp_value) in
  DB.find req data

let find_opt : 'a . (module Caqti_lwt.CONNECTION) ->
  ('a,[< `One | `Zero ]) Request.t -> ('a option, [> Caqti_error.call_or_retrieve ]) result Lwt.t =
  fun (module DB: Caqti_lwt.CONNECTION) (type a) ((MkCaqti (inps,req),wrapp_value): (a,_) Request.t) ->
  let data = Request.unwrap (inps,wrapp_value) in
  DB.find_opt req data

let collect_list : 'a . (module Caqti_lwt.CONNECTION) ->
  ('a,[< `Many | `One | `Zero ]) Request.t -> ('a list, [> Caqti_error.call_or_retrieve ]) result Lwt.t =
  fun (module DB: Caqti_lwt.CONNECTION) (type a) ((MkCaqti (inps,req),wrapp_value): (a,_) Request.t) ->
  let data = Request.unwrap (inps,wrapp_value) in
  DB.collect_list req data


module StaticDatabase = struct

  type wrapped_table =
      MkTable : int * string * 'a Schema.table * [ `Table ] Schema.constraint_ list -> wrapped_table

  type t = (int, wrapped_table) Hashtbl.t

  let init () : t = Hashtbl.create 10

  let declare_table tables ?(constraints : _ list =[]) ~name tbl =
    List.iter Schema.ensure_table_constraint constraints;
    let id = Hashtbl.length tables in
    Hashtbl.add tables id (MkTable (id, name, tbl, constraints));
    let rec to_table : 'a . string -> 'a Schema.table -> 'a Expr.expr_list =
      fun name (type a) (table: a Schema.table) : a Expr.expr_list ->
        match table with
        | [] -> []
        | (field_name, field_ty, _) :: rest ->
          ((FIELD ((id, name), field_name,field_ty)) : _ Expr.t)
          :: to_table name rest in
    let table = to_table name tbl in
    (id, name), table

  let initialise tables (module DB: Caqti_lwt.CONNECTION) =
    let open Lwt_result.Syntax in
    let table_defs =
      Hashtbl.fold
        (fun _key (MkTable (_, name, table, constraints)) acc ->
           List.cons (Schema.to_sql ~name table constraints) acc
        ) tables ([]: 'a list) in
    let* _ = 
      Lwt_list.map_s (fun table_def ->
        let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) table_def in
        DB.exec req ()
      ) table_defs
      |> Lwt.map result_all_unit in
    Lwt_result.return ()

end


module VersionedDatabase = struct


  type version = int list

  type migration = (unit, unit, [`Zero]) Caqti_request.t

  type wrapped_table =
      MkTable : int * string * (version * migration) list * 'a Schema.table * [ `Table ] Schema.constraint_ list -> wrapped_table

  type t = {
    version: version;
    tables: (int, wrapped_table) Hashtbl.t;
    migrations: (version * migration list) list;
    version_db: StaticDatabase.t;
    version_table_name: table_name;
    version_table_field: string Expr.t;
  }

  let version ls : version = ls
  let compare_version = List.compare Int.compare
  let order_by_version ls =
    List.sort (fun (vl, _) (vr, _) ->
      compare_version vl vr) ls
  let find_migrations_to_run ~current_version ls =
    drop_while ~f:(fun (ver, _) -> compare_version ver current_version <= 0) ls

  let init ?(migrations=[]) version ~name =
    let migrations = order_by_version migrations in
    let version_db = StaticDatabase.init () in
    let version_table_name, Expr.[version_table_field] =
      StaticDatabase.declare_table version_db ~name:("petrol_" ^ name ^ "_version_db") Schema.[
        field ~constraints:[primary_key (); not_null ()] "version" ~ty:Type.TEXT
      ] in
    {
      version;
      tables=Hashtbl.create 10;
      migrations;
      version_db;
      version_table_name; version_table_field;
    }


  let declare_table t ?(constraints : _ list =[]) ?(migrations=[]) ~name tbl =
    List.iter Schema.ensure_table_constraint constraints;
    let id = Hashtbl.length t.tables in
    let migrations = order_by_version migrations in
    Hashtbl.add t.tables id (MkTable (id, name, migrations, tbl, constraints));
    let rec to_table : 'a . string -> 'a Schema.table -> 'a Expr.expr_list =
      fun name (type a) (table: a Schema.table) : a Expr.expr_list ->
        match table with
        | [] -> []
        | (field_name, field_ty, _) :: rest ->
          ((FIELD ((id, name), field_name,field_ty)) : _ Expr.t)
          :: to_table name rest in
    let table = to_table name tbl in
    (id, name), table

  let get_current_version t con =
    let open Lwt_result.Syntax in
    let* () = StaticDatabase.initialise t.version_db con in
    let* res =
      Query.select Expr.[t.version_table_field] ~from:t.version_table_name
      |> Request.make_zero_or_one
      |> find_opt con in
    let version =
      Option.map fst res
      |> Option.map (fun s -> s |> String.split_on_char '.' |> List.map int_of_string)
      |> Option.value ~default:t.version
      |> Result.ok in
    Lwt.return version

  let set_version t version con =
    let open Lwt_result.Syntax in
    let* () = StaticDatabase.initialise t.version_db con in
    let version_str = String.concat "." (List.map Int.to_string version) in
    let (module DB: Caqti_lwt.CONNECTION) = con in
    let* () =
      Query.delete ~from:t.version_table_name
      |> Request.make_zero
      |> exec con in
    let* () =
      Query.insert ~table:t.version_table_name
        ~values:Expr.[t.version_table_field := s version_str]
      |> Request.make_zero
      |> exec con in
    Lwt.return_ok ()

  let migrations_needed t ((module DB: Caqti_lwt.CONNECTION) as conn) =
    let open Lwt_result.Syntax in
    let* current_version = get_current_version t conn in
    match compare_version current_version t.version with
    | 0 -> Lwt.return_ok false
    | v when v < 0 -> Lwt.return_ok true
    | _ ->
      Lwt.return_error (`Newer_version_than_supported current_version)

  let initialise t ((module DB: Caqti_lwt.CONNECTION) as conn) =
    let open Lwt_result.Syntax in
    let* current_version = get_current_version t conn in
    match compare_version current_version t.version with
    | 0 -> (* same version *)
      (* collect queries *)
      let table_defs =
        Hashtbl.fold
          (fun _key (MkTable (_, name, _, table, constraints)) acc ->
             List.cons (Schema.to_sql ~name table constraints) acc
          ) t.tables ([]: 'a list) in
      (* execute them *)
      let* _ = 
        Lwt_list.map_s (fun table_def ->
          let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) table_def in
          DB.exec req ()
        ) table_defs
        |> Lwt.map result_all_unit in
      (* done *)
      Lwt_result.return ()
    | v when v < 0 ->           (* version on db is older, migrations needed *)
      DB.with_transaction begin fun () -> 
        let migrations =
          find_migrations_to_run ~current_version t.migrations in
        (* first run global migrations *)
        let* () =
          Lwt_list.map_s (fun (_, migrations) ->
            Lwt_list.map_s (fun migration ->
              DB.exec migration ()
            ) migrations
            |> Lwt.map result_all_unit
          ) migrations
          |> Lwt.map result_all_unit in
        (* then, for each table *)
        let* () = 
          Hashtbl.to_seq_values t.tables
          |> Lwt_seq.of_seq
          |> Lwt_seq.map_s (fun (MkTable (_, _, migrations, _, _)) ->
            (* find all migrations from the stored version to the applications version  *)
            let migrations = find_migrations_to_run ~current_version migrations in
            (* run them in order *)
            Lwt_list.map_s (fun (_, migration) ->
              DB.exec migration ()
            ) migrations
            |> Lwt.map result_all_unit)
          |> Lwt_seq.to_list
          |> Lwt.map result_all_unit in
        let* () = set_version t t.version conn in
        Lwt.return_ok ()
      end
    | _ ->
      Lwt.return_error (`Newer_version_than_supported current_version)

end

