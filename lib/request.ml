type ('res, !'multiplicity) caqti_request_inner =
    MkCaqti:'input Type.ty_list * ('input, 'res, 'multiplicity) Caqti_request.t ->
      ('res, 'multiplicity) caqti_request_inner

type ('res, !'multiplicity) t =
  ('res, 'multiplicity) caqti_request_inner * Expr.wrapped_value list

type wrapped_ty_list = MkWrappedTyList : 'a Type.ty_list -> wrapped_ty_list

let rec extract_ty_list : Expr.wrapped_value list -> wrapped_ty_list =
  function
  | [] -> MkWrappedTyList Nil
  | Expr.MkWrapped (ty, _) :: rest ->
    let (MkWrappedTyList rest) = extract_ty_list rest in
    MkWrappedTyList (Cons (ty, rest))

let rec unwrap : 'a . 'a Type.ty_list * Expr.wrapped_value list -> 'a =
  fun (type a) ((tyls: a Type.ty_list), (ls: Expr.wrapped_value list)) : a ->
  match tyls,ls with
  | Nil,[] -> ()
  | Cons (NULLABLE ty, tyls), Expr.MkWrapped (NULLABLE oty, vl) :: ls ->
    begin match Type.eq_ty (ty,oty) with
    | Some Refl -> (vl, unwrap (tyls, ls))
    | None ->
      Format.ksprintf failwith "wrapped value list did not conform to specification - expected %s got %s"
        (Type.show ty) (Type.show oty)
    end
  | Cons (INTEGER, tyls), Expr.MkWrapped (INTEGER, vl) :: ls  ->
    (vl, unwrap (tyls, ls))
  | Cons (REAL, tyls), Expr.MkWrapped (REAL, vl) :: ls  ->
    (vl, unwrap (tyls, ls))
  | Cons (TEXT, tyls), Expr.MkWrapped (TEXT, vl) :: ls  ->
    (vl, unwrap (tyls, ls))
  | Cons (CUSTOM {eq_witness;_} as ty, tyls), Expr.MkWrapped (CUSTOM {witness;_} as oty, vl) :: ls ->
    begin match eq_witness.eq witness with
    | Some Refl -> (vl, unwrap (tyls, ls))
    | None ->
      Format.ksprintf failwith "wrapped value list did not conform to specification - expected %s got %s"
        (Type.show ty) (Type.show oty)
    end
  | Cons (ty, _), Expr.MkWrapped (oty, _) :: _  ->
    Format.ksprintf failwith "wrapped value list did not conform to specification - expected %s got %s"
      (Type.show ty) (Type.show oty)
  | Nil, _ | _, [] -> failwith "wrapped value list length mismatch"

module QueryMap = Type.Map (struct type ('a,'b) t = ('a,'b) caqti_request_inner end)

let cache_zero : (string, [ `Zero ] QueryMap.t) Hashtbl.t = Hashtbl.create 10
let cache_one : (string, [ `One ] QueryMap.t) Hashtbl.t = Hashtbl.create 10
let cache_zero_or_one : (string, [ `Zero | `One ] QueryMap.t) Hashtbl.t = Hashtbl.create 10
let cache_many : (string, [ `Many | `Zero | `One ] QueryMap.t) Hashtbl.t = Hashtbl.create 10

let make_zero : 'b . (unit,'b) Query.t -> (unit, [`Zero]) t =
  fun (type b) (query: (unit,b) Query.t) : (unit, [`Zero]) t ->
  let query_repr = Caqti_query.of_string_exn (Format.asprintf "%a" Query.pp query) in
  let query_values = Query.query_values query in
  (* Format.printf "DEBUG: query is: %a, values are [%a]@.%!" Caqti_query.pp query_repr
   *   (Format.pp_print_list Expr.pp_wrapped_value) query_values; *)
  let (MkWrappedTyList query_value_ty) = extract_ty_list query_values in
  let request = Caqti_request.create (Type.ty_list_to_caqti_ty query_value_ty) Caqti_type.unit Caqti_mult.zero
                  (fun _ -> query_repr) in
  MkCaqti (query_value_ty,request), query_values

let make_zero : 'b . (unit,'b) Query.t -> (unit, [`Zero]) t =
  fun (type b) (query: (unit,b) Query.t) : (unit, [`Zero]) t ->
  let query_txt = Format.asprintf "%a" Query.pp query in
  let query_ty = Query.query_ret_ty query in
  let query_values = Query.query_values query in
  let ty_map = match Hashtbl.find_opt cache_zero query_txt with
    | Some ty_map -> ty_map
    | None -> QueryMap.empty in
  match QueryMap.lookup_opt ty_map ~key:query_ty with
  | Some res -> res, query_values
  | None ->
    let (query_inner, _) as query = make_zero query in
    let ty_map = QueryMap.insert ty_map ~key:query_ty ~data:query_inner in
    Hashtbl.replace cache_zero query_txt ty_map;
    query

let make_one : 'a 'b . ('a,'b) Query.t -> ('a, [`One]) t =
  fun (type a b) (query: (a,b) Query.t) : (a, [`One]) t ->
  let query_repr = Caqti_query.of_string_exn (Format.asprintf "%a" Query.pp query) in
  let query_values = Query.query_values query in
  let (MkWrappedTyList query_value_ty) = extract_ty_list query_values in
  let ret_ty = Query.query_ret_ty query in
  let request = Caqti_request.create (Type.ty_list_to_caqti_ty query_value_ty) (Type.ty_list_to_caqti_ty ret_ty)
                  Caqti_mult.one (fun _ -> query_repr) in
  MkCaqti (query_value_ty,request), query_values

let make_one : 'a 'b . ('a,'b) Query.t -> ('a, [`One]) t =
  fun (type a b) (query: (a,b) Query.t) : (a, [`One]) t ->
  let query_txt = Format.asprintf "%a" Query.pp query in
  let query_ty = Query.query_ret_ty query in
  let query_values = Query.query_values query in
  let ty_map = match Hashtbl.find_opt cache_one query_txt with
    | Some ty_map -> ty_map
    | None -> QueryMap.empty in
  match QueryMap.lookup_opt ty_map ~key:query_ty with
  | Some res -> res, query_values
  | None ->
    let (query_values, _) as query = make_one query in
    let ty_map = QueryMap.insert ty_map ~key:query_ty ~data:query_values in
    Hashtbl.replace cache_one query_txt ty_map;
    query

let make_zero_or_one : 'a 'b . ('a,'b) Query.t -> ('a, [`Zero | `One]) t =
  fun (type a b) (query: (a,b) Query.t) : (a, [`Zero | `One]) t ->
  let query_repr = Caqti_query.of_string_exn (Format.asprintf "%a" Query.pp query) in
  let query_values = Query.query_values query in
  let (MkWrappedTyList query_value_ty) = extract_ty_list query_values in
  let ret_ty = Query.query_ret_ty query in
  let request = Caqti_request.create (Type.ty_list_to_caqti_ty query_value_ty) (Type.ty_list_to_caqti_ty ret_ty)
                  Caqti_mult.zero_or_one (fun _ -> query_repr) in
  MkCaqti (query_value_ty,request), query_values

let make_zero_or_one : 'a 'b . ('a,'b) Query.t -> ('a, [`Zero | `One]) t =
  fun (type a b) (query: (a,b) Query.t) : (a, [`Zero | `One]) t ->
  let query_txt = Format.asprintf "%a" Query.pp query in
  let query_ty = Query.query_ret_ty query in
  let query_values = Query.query_values query in
  let ty_map = match Hashtbl.find_opt cache_zero_or_one query_txt with
    | Some ty_map -> ty_map
    | None -> QueryMap.empty in
  match QueryMap.lookup_opt ty_map ~key:query_ty with
  | Some res -> res, query_values
  | None ->
    let (query_inner, _) as query = make_zero_or_one query in
    let ty_map = QueryMap.insert ty_map ~key:query_ty ~data:query_inner in
    Hashtbl.replace cache_zero_or_one query_txt ty_map;
    query

let make_many : 'a 'b . ('a,'b) Query.t -> ('a, [`Many | `Zero | `One]) t =
  fun (type a b) (query: (a,b) Query.t) : (a, [`Many | `Zero | `One]) t ->
  let query_repr = Caqti_query.of_string_exn (Format.asprintf "%a" Query.pp query) in
  let query_values = Query.query_values query in
  let (MkWrappedTyList query_value_ty) = extract_ty_list query_values in
  let ret_ty = Query.query_ret_ty query in
  let request = Caqti_request.create (Type.ty_list_to_caqti_ty query_value_ty) (Type.ty_list_to_caqti_ty ret_ty)
                  Caqti_mult.zero_or_more (fun _ -> query_repr) in
  MkCaqti (query_value_ty,request), query_values

let make_many : 'a 'b . ('a,'b) Query.t -> ('a, [`Many | `Zero | `One]) t =
  fun (type a b) (query: (a,b) Query.t) : (a, [`Many | `Zero | `One]) t ->
  let query_txt = Format.asprintf "%a" Query.pp query in
  let query_ty = Query.query_ret_ty query in
  let query_values = Query.query_values query in
  let ty_map = match Hashtbl.find_opt cache_many query_txt with
    | Some ty_map -> ty_map
    | None -> QueryMap.empty in
  match QueryMap.lookup_opt ty_map ~key:query_ty with
  | Some res -> res, query_values
  | None ->
    let (query_inner, _) as query = make_many query in
    let ty_map = QueryMap.insert ty_map ~key:query_ty ~data:query_inner in
    Hashtbl.replace cache_many query_txt ty_map;
    query


