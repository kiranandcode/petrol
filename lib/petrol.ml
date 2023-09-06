module Schema = Schema
module Query = Query
module Expr = Expr

module Type = struct
  include Type

  let pp fmt vl = Format.fprintf fmt "%s" (show vl)
end


type table_name = Types.table_name
type ('ret_ty, 'query_kind) query = ('ret_ty, 'query_kind) Types.query


type ('res,'multiplicity) request = ('res,'multiplicity) Request.t


module Request = Request


module Sqlite3 = struct

  module Request = Request

  module Expr = struct
    type 'a t = 'a Expr.t

    type 'a expr_list = 'a Expr.expr_list =
      | [] : unit expr_list
      | (::) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

    type wrapped_assign = Types.wrapped_assign

    include Expr.Sqlite3
  end

  module Type = struct

    type 'a t = 'a Type.t

    let int = Type.int
    let real = Type.real
    let text = Type.text
    let bool = Type.bool
    let null_ty = Type.null_ty

    include Type.Sqlite3

    module Numeric = Type.Numeric

  end

end

module Postgres = struct

  module Request = Request

  module Expr = struct
    type 'a t = 'a Expr.t

    type 'a expr_list = 'a Expr.expr_list =
      | [] : unit expr_list
      | (::) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

    type wrapped_assign = Types.wrapped_assign

    include Expr.Postgres
  end

  module Type = struct

    type 'a t = 'a Type.t

    let int = Type.int
    let real = Type.real
    let text = Type.text
    let bool = Type.bool

    include Type.Postgres

    module Numeric = Type.Numeric

  end

end


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

module StaticSchema = struct

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
          ((Types.FIELD ((id, name), field_name,field_ty)) : _ Expr.t)
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
    let* () = 
      Lwt_list.map_s (fun table_def ->
          let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) table_def in
          DB.exec req ()
        ) table_defs
      |> Lwt.map result_all_unit in
    Lwt_result.return ()

end


module VersionedSchema = struct

  type version = int list

  type migration = (unit, unit, [`Zero]) Caqti_request.t

  type wrapped_table =
      MkTable : int * string * version option *
                (version * migration list) list *
                'a Schema.table * [ `Table ] Schema.constraint_ list -> wrapped_table

  type t = {
    version: version;
    tables: (int, wrapped_table) Hashtbl.t;
    migrations: (version * migration list) list;
    version_db: StaticSchema.t;
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
    let version_db = StaticSchema.init () in
    let version_table_name, Expr.[version_table_field] =
      StaticSchema.declare_table version_db ~name:("petrol_" ^ name ^ "_version_db") Schema.[
          field ~constraints:[primary_key (); not_null ()] "version" ~ty:Type.TEXT
        ] in
    {
      version;
      tables=Hashtbl.create 10;
      migrations;
      version_db;
      version_table_name; version_table_field;
    }

  let declare_table t ?since ?(constraints : _ list =[]) ?(migrations=[]) ~name tbl =
    List.iter Schema.ensure_table_constraint constraints;
    let id = Hashtbl.length t.tables in
    let migrations = order_by_version migrations in
    Hashtbl.add t.tables id (MkTable (id, name, since, migrations, tbl, constraints));
    let rec to_table : 'a . string -> 'a Schema.table -> 'a Expr.expr_list =
      fun name (type a) (table: a Schema.table) : a Expr.expr_list ->
        match table with
        | [] -> []
        | (field_name, field_ty, _) :: rest ->
          ((Types.FIELD ((id, name), field_name,field_ty)) : _ Expr.t)
          :: to_table name rest in
    let table = to_table name tbl in
    (id, name), table

  let set_version t version con =
    let open Lwt_result.Syntax in
    let* () = StaticSchema.initialise t.version_db con in
    let version_str = String.concat "." (List.map Int.to_string version) in
    let (module DB: Caqti_lwt.CONNECTION) = con in
    let* () =
      Query.delete ~from:t.version_table_name
      |> Request.make_zero
      |> exec con in
    let* () =
      Query.insert ~table:t.version_table_name
        ~values:Expr.Common.[t.version_table_field := s version_str]
      |> Request.make_zero
      |> exec con in
    Lwt.return_ok ()

  let get_current_version t con =
    let open Lwt_result.Syntax in
    let* () = StaticSchema.initialise t.version_db con in
    let* res =
      Query.select Expr.[t.version_table_field] ~from:t.version_table_name
      |> Request.make_zero_or_one
      |> find_opt con in
    match res with
    | None ->
      let* () = set_version t t.version con in
      Lwt.return_ok t.version
    | Some (old_version, ()) ->
      let old_version = old_version |> String.split_on_char '.' |> List.map int_of_string in
      Lwt.return_ok old_version

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
          (fun key (MkTable (_, name,_,  _, table, constraints)) acc ->
             List.cons (key, Schema.to_sql ~name table constraints) acc
          ) t.tables ([]: 'a list)
        |> List.sort (fun (k1, _) (k2,_) -> Int.compare k1 k2) in
      (* execute them *)
      let* () = 
        Lwt_list.map_s (fun (_, table_def) ->
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
        (* first, create any tables that aren't present yet *)
        let* () =
          Hashtbl.to_seq t.tables
          |> List.of_seq
          |> List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2)
          |> Lwt_seq.of_list
          |> Lwt_seq.map_s (fun (_, MkTable (_, name, since, _, table, constraints)) ->
              match since with
              | Some since when compare_version current_version since < 0 ->
                let table_def = Schema.to_sql ~name table constraints in
                let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) table_def in
                DB.exec req ()
              | _ ->
                (* if since is not present, or current version is
                   greater than or equal to since, then the table is
                   already present *)
                Lwt.return_ok ())
          |> Lwt_seq.to_list
          |> Lwt.map result_all_unit in
        (* then, run global migrations *)
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
          Hashtbl.to_seq t.tables
          |> List.of_seq
          |> List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2)
          |> Lwt_seq.of_list
          |> Lwt_seq.map_s (fun (_, MkTable (_, _, since, migrations, _, _)) ->
              match since with
              | Some since when compare_version current_version since < 0 ->
                Lwt.return_ok ()
              | _ ->
                (* find all migrations from the stored version to the applications version  *)
                let migrations = find_migrations_to_run ~current_version migrations in
                (* run them in order *)
                Lwt_list.map_s (fun (_, migrations) ->
                    Lwt_list.map_s (fun migration ->
                        DB.exec migration ()
                      ) migrations
                    |> Lwt.map result_all_unit
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
