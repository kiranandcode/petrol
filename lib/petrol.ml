module Type = Type
module Schema = Schema
module Expr = Expr
module Query = Query
module Request = Request

type table_name = Types.table_name

let result_all : ('a, 'e) result list -> ('a list, 'e) result =
  fun ls ->
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok (h) :: t -> loop (h :: acc) t
    | Error err :: _ -> Error err in
  loop [] ls

module Database = struct

  type wrapped_table =
      MkTable : int * string * 'a Schema.table * [ `Table ] Schema.constraint_ list -> wrapped_table

  let tables = Hashtbl.create 10

  let declare_table ?(constraints : _ list =[]) ~name tbl =
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

  let initialise (module DB: Caqti_lwt.CONNECTION) =
    let open Lwt_result.Syntax in
    let table_defs =
      Hashtbl.fold
        (fun _key (MkTable (_, name, table, constraints)) acc ->
           List.cons (Schema.to_sql ~name table constraints) acc
        ) tables ([]: 'a list) in
    let* _ = 
      Lwt_list.map_s (fun table_def ->
        let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) table_def in
        DB.exec req () |> Lwt_result.map_error (fun err -> `Msg (Caqti_error.show err))
      ) table_defs
      |> Lwt.map result_all in
    Lwt_result.return ()


  module Versioned = struct
  end
end


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

