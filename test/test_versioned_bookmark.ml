[@@@warning "-27-26"]
open Lwt_result.Syntax

let v_1_0_0 = Petrol.VersionedSchema.version [1;0;0]
let v_1_2_0 = Petrol.VersionedSchema.version [1;2;0]
let v_2 = Petrol.VersionedSchema.version [2]

let result_all_unit : (unit, 'e) result list -> (unit, 'e) result =
  fun ls ->
  let rec loop = function
    | [] -> Ok ()
    | Ok () :: t -> loop t
    | Error err :: _ -> Error err in
  loop ls

let result_all : ('a, 'e) result list -> ('a list, 'e) result =
  fun ls ->
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok (h) :: t -> loop (h :: acc) t
    | Error err :: _ -> Error err in
  loop [] ls


module Bookmark = struct

  type t = {id: int; age: int; name: string; url: string; tags: string list}

  let show {id;age;name;url;tags} =
    Format.sprintf "bookmark \"%s\": %s\n\tage: %d\n\ttags: %s" name url age
      (String.concat "," tags)

  module type BOOKMARKS = sig
    val db : Petrol.VersionedSchema.t

    val insert :
      t -> (module Caqti_lwt.CONNECTION) ->
      (unit, [> Caqti_error.call_or_retrieve ]) Lwt_result.t

    val update_age :
      id:int ->
      age:int ->
      (module Caqti_lwt.CONNECTION) ->
      (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

    val collect_all :
      (module Caqti_lwt.CONNECTION) ->
      (t list, [> Caqti_error.call_or_retrieve ]) Lwt_result.t

    val collect_since :
      age:int ->
      (module Caqti_lwt.CONNECTION) ->
      (t list, [> Caqti_error.call_or_retrieve ]) Lwt_result.t

    val add_tag :
      id:int ->
      tag:string ->
      (module Caqti_lwt.CONNECTION) ->
      (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

    val clear_tags :
      id:int ->
      (module Caqti_lwt.CONNECTION) ->
      (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

    val delete :
      id:int ->
      (module Caqti_lwt.CONNECTION) ->
      (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

  end

  module V1 = struct
    let db = Petrol.VersionedSchema.init_sqlite3 v_1_0_0 ~name:"bookmark" 

    open Petrol
    open Petrol.Sqlite3

    let t, Expr.[id;url] =
      VersionedSchema.declare_table db ~name:"person"
        Schema.[
          field ~constraints:[primary_key ~name:"bookmark_id" ()] "id" ~ty:Type.int;
          field "url" ~ty:Type.text;
        ]

    let insert {id=id';url=u; _} db =
      Query.insert ~table:t ~values:Expr.[id := i id'; url := s u]
      |> Request.make_zero
      |> Petrol.exec db

    let collect_all db =
      Query.select Expr.[id;url] ~from:t
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map (List.map (fun (id, (url, _)) -> {id; age= -1; name=""; url;tags=[]}))

    let delete ~id:id' db =
      Query.delete ~from:t
      |> Query.where Expr.(id = i id')
      |> Request.make_zero
      |> Petrol.exec db

    let update_age ~id ~age db = failwith "update_age not supported"

    let collect_since  ~age db = failwith "collect_since not supported"

    let add_tag ~id ~tag db = failwith "add_tag not supported"

    let clear_tags ~id db = failwith "clear_tags not supported"

  end

  module V1_2  = struct
    let db = Petrol.VersionedSchema.init_sqlite3 v_1_2_0 ~name:"bookmark" 

    open Petrol
    open Petrol.Sqlite3

    let t, Expr.[id;age;url] =
      VersionedSchema.declare_table db ~name:"person"
        Schema.[
          field ~constraints:[primary_key ~name:"bookmark_id" ()] "id" ~ty:Type.int;
          field "age" ~ty:Type.int;
          field "url" ~ty:Type.text;
        ]
        ~migrations:[v_1_2_0, [
          Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit)
            {sql|ALTER TABLE person ADD COLUMN age INTEGER DEFAULT 1000|sql}
        ]]

    let insert {id=id';age=age';url=u;_} db =
      Query.insert ~table:t ~values:Expr.[id := i id'; age := i age'; url := s u]
      |> Request.make_zero
      |> Petrol.exec db

    let update_age ~id:id' ~age:age' db =
      Query.update ~table:t ~set:Expr.[age := i age']
      |> Query.where Expr.(id = i id')
      |> Request.make_zero
      |> Petrol.exec db

    let collect_all db =
      Query.select Expr.[id;age;url] ~from:t
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map (List.map (fun (id, (age, (url, _))) -> {id; age; name=""; url;tags=[]}))

    let collect_since ~age:age' db =
      Query.select Expr.[id;age;url] ~from:t
      |> Query.where Expr.(age > i age')
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map (List.map (fun (id, (age, (url, _))) -> {id; age; name=""; url;tags=[]}))

    let delete ~id:id' db =
      Query.delete ~from:t
      |> Query.where Expr.(id = i id')
      |> Request.make_zero
      |> Petrol.exec db

    let add_tag ~id ~tag db = failwith "add_tag not supported"

    let clear_tags ~id db = failwith "clear_tags not supported"

  end

  module V2 = struct
    let db = Petrol.VersionedSchema.init_sqlite3 v_2 ~name:"bookmark" 

    open Petrol
    open Petrol.Sqlite3

    let t, Expr.[id;age;name;url] =
      VersionedSchema.declare_table db ~name:"person"
        Schema.[
          field ~constraints:[primary_key ~name:"bookmark_id" ()] "id" ~ty:Type.int;
          field "age" ~ty:Type.int;
          field "name" ~ty:Type.text;
          field "url" ~ty:Type.text;
        ]
        ~migrations:[
          v_1_2_0, [
            Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit)
              {sql|ALTER TABLE person ADD COLUMN age INTEGER DEFAULT 1000|sql}
          ];
          v_2, [
            Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit)
              {sql|ALTER TABLE person ADD COLUMN name STRING DEFAULT unnamed|sql}
          ];
        ]

    module Tags = struct

      let t, Expr.[_tag_id;bookmark_id; tag_text] =
        VersionedSchema.declare_table db ~name:"tag" ~since:v_2
          Schema.[
            field ~constraints:[primary_key ~auto_increment:true ()] "tag_id" ~ty:Type.int;
            field "bookmark_id" ~ty:Type.int ~constraints:[
              foreign_key ~table:t ~columns:[id] ()
            ];
            field "tag_text" ~ty:Type.text;
          ] 

      let collect ~id:id' db =
        Query.select Expr.[tag_text] ~from:t
        |> Query.where Expr.(bookmark_id = i id')
        |> Request.make_many
        |> Petrol.collect_list db
        |> Lwt_result.map (List.map (fun (text, ()) -> text))

      let insert ~id:id' tag db =
        Query.insert ~table:t ~values:Expr.[bookmark_id := i id'; tag_text := s tag]
        |> Request.make_zero
        |> Petrol.exec db

      let insert_all ~id tags db =
        Lwt_list.map_s (fun tag -> insert ~id tag db) tags
        |> Lwt.map result_all_unit

      let clear ~id:id' db =
        Query.delete ~from:t
        |> Query.where Expr.(bookmark_id = i id')
        |> Request.make_zero
        |> Petrol.exec db

    end

    let insert {id=id';name=name';age=age';url=u;tags=tags} db =
      let* () = 
        Query.insert ~table:t ~values:Expr.[id := i id'; age := i age'; url := s u; name := s name']
        |> Request.make_zero
        |> Petrol.exec db in
      Tags.insert_all ~id:id' tags db

    let update_age ~id:id' ~age:age' db =
      Query.update ~table:t ~set:Expr.[age := i age']
      |> Query.where Expr.(id = i id')
      |> Request.make_zero
      |> Petrol.exec db

    let collect_all db =
      let* bookmarks =
        Query.select Expr.[id;name;age;url] ~from:t
        |> Request.make_many
        |> Petrol.collect_list db
        |> Lwt_result.map (List.map (fun (id, (name, (age, (url, _)))) -> {id; age; name; url;tags=[]})) in
      Lwt_seq.of_list bookmarks
      |> Lwt_seq.map_s (fun t ->
        let* tags = Tags.collect ~id:t.id db in
        Lwt_result.return ({t with tags})
      )
      |> Lwt_seq.to_list
      |> Lwt.map result_all

    let collect_since ~age:age' db =
      Query.select Expr.[id;name;age;url] ~from:t
      |> Query.where Expr.(age > i age')
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map (List.map (fun (id, (name, (age, (url, _)))) -> {id; age; name; url;tags=[]}))

    let add_tag ~id:id' ~tag db =
      Tags.insert ~id:id' tag db

    let clear_tags ~id db =
      Tags.clear ~id db

    let delete ~id:id' db =
      Query.delete ~from:t
      |> Query.where Expr.(id = i id')
      |> Request.make_zero
      |> Petrol.exec db

  end

end

let () =
  Test_utils.main begin fun conn -> fun (ver,args) ->
    let (module DB) = match[@warning "-8"] ver with
      | "v1" -> ((module Bookmark.V1: Bookmark.BOOKMARKS))
      | "v1.2" -> ((module Bookmark.V1_2: Bookmark.BOOKMARKS))
      | "v2" -> ((module Bookmark.V2: Bookmark.BOOKMARKS)) in

    match[@warning "-8"] List.hd args, List.tl args with
    | "init", _ ->
      let* _ = Petrol.VersionedSchema.initialise DB.db conn in
      Lwt.return_ok ()
    | "add", (id :: url :: rest) ->
      let id = int_of_string id in
      let age =
        List.nth_opt rest 0
        |> Option.value ~default:"-1"
        |> int_of_string in
      let name =
        List.nth_opt rest 1
        |> Option.value ~default:"" in
      let tags = match rest with (_ :: _ :: rest) -> rest | _ -> [] in
      let b = Bookmark.{id;name;url;age;tags} in
      let* _ = Petrol.VersionedSchema.initialise DB.db conn in
      let* _ = DB.insert b conn in
      Lwt.return_ok ()
    |  "update-age", [id;age] ->
      let id = int_of_string id in
      let age = int_of_string age in
      let* _ = Petrol.VersionedSchema.initialise DB.db conn in
      let* _ = DB.update_age ~id ~age conn in
      Lwt.return_ok ()
    |  "list", _ ->
      let* _ = Petrol.VersionedSchema.initialise DB.db conn in
      let* result = DB.collect_all conn in
      List.iteri (fun ind b ->
        print_endline ("[" ^ string_of_int ind ^ "] - " ^ Bookmark.show b)
      ) result;
      Lwt.return_ok ()
    |  "since", [age] ->
      let age = int_of_string age in
      let* _ = Petrol.VersionedSchema.initialise DB.db conn in
      let* result = DB.collect_since ~age conn in
      List.iteri (fun ind b ->
        print_endline ("[" ^ string_of_int ind ^ "] - " ^ Bookmark.show b)
      ) result;
      Lwt.return_ok ()
    |  "add-tag", [id;tag] ->
      let id = int_of_string id in
      let* _ = Petrol.VersionedSchema.initialise DB.db conn in
      let* () = DB.add_tag ~id ~tag conn in
      Lwt.return_ok ()
    |  "delete", [id] ->
      let id = int_of_string id in
      let* _ = Petrol.VersionedSchema.initialise DB.db conn in
      let* () = DB.delete ~id conn in
      Lwt.return_ok ()
    |  "clear-tags", [id] ->
      let id = int_of_string id in
      let* _ = Petrol.VersionedSchema.initialise DB.db conn in
      let* () = DB.clear_tags ~id conn in
      Lwt.return_ok ()
  end
