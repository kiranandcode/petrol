type (_,_) eq = Refl: ('a,'a) eq

type 'a witness = ..
type 'a eq_witness = {eq: 'b . 'b witness -> ('a, 'b) eq option }
type 'a t = 
  | NULLABLE: 'a t -> 'a option t
  | INTEGER : int t
  | REAL: float t
  | TEXT : string t
  | BOOLEAN: bool t
  | CUSTOM : {
      ty: 'a Caqti_type.t;
      witness: 'a witness;
      eq_witness: 'a eq_witness;
      repr: string;
    } -> 'a t

let custom : 'a . ty:'a Caqti_type.t -> repr:string -> 'a t =
  fun (type a) ~(ty:a Caqti_type.t) ~repr : a t ->
  let module M = struct
    type _ witness += CUSTOM : a witness
    end in
  let eq_witness : 'b . 'b witness -> (a, 'b) eq option =
    fun (type b) (witness: b witness) : (a, b) eq option ->
      match witness with
      | M.CUSTOM -> Some Refl
      | _ -> None in
  CUSTOM {
    ty;
    witness=M.CUSTOM;
    eq_witness={eq=eq_witness};
    repr;
  }

type _ ty_list =
  | Nil : unit ty_list
  | Cons : 'a t * 'b ty_list -> ('a * 'b) ty_list

module Numeric = struct

  type 'a integral = Int : int integral | Int32 : int32 integral | Int64 : int64 integral

  type 'a t = Int: int t | Int32 : int32 t | Int64 : int64 t | Real: float t

end

module Postgres = struct

  type 'a witness += BIGINT : int64 witness
  let big_int = CUSTOM { ty = Caqti_type.int64; repr = "BIGINT"; witness=BIGINT;
                         eq_witness={eq=
                                       fun (type b) (witness: b witness) : (int64, b) eq option ->
                                         match witness with
                                         | BIGINT -> Some Refl
                                         | _ -> None
                                    }}

  type 'a witness += BIGSERIAL : int64 witness
  let big_serial = CUSTOM { ty = Caqti_type.int64; repr = "BIGSERIAL"; witness=BIGSERIAL;
                            eq_witness={eq=fun (type b) (witness: b witness) : (int64, b) eq option ->
                              match witness with
                              | BIGSERIAL -> Some Refl
                              | _ -> None}}
  type 'a witness += BYTEA : string witness
  let bytea  = CUSTOM { ty = Caqti_type.string; repr = "BYTEA"; witness=BYTEA;
                        eq_witness={eq=fun (type b) (witness: b witness) : (string, b) eq option ->
                          match witness with
                          | BYTEA -> Some Refl
                          | _ -> None}}

  type 'a witness += CHARACTER : int -> string witness
  let character (n: int) = CUSTOM { ty = Caqti_type.string; repr = Format.sprintf "CHARACTER(%d)" n; witness=CHARACTER n;
                                    eq_witness={eq=fun (type b) (witness: b witness) : (string, b) eq option ->
                                      match witness with
                                      | CHARACTER n' when n = n' -> Some Refl
                                      | _ -> None}}
  type 'a witness += CHARACTER_VARYING : int -> string witness
  let character_varying n = CUSTOM { ty = Caqti_type.string; repr = Format.sprintf "CHARACTER VARYING(%d)" n; witness=CHARACTER_VARYING n;
                                     eq_witness={eq=fun (type b) (witness: b witness) : (string, b) eq option ->
                                       match witness with
                                       | CHARACTER_VARYING n' when n = n' -> Some Refl
                                       | _ -> None}}

  type 'a witness += DATE : Ptime.t witness
  let date = CUSTOM { ty = Caqti_type.Std.pdate; repr = "DATE"; witness=DATE;
                      eq_witness={eq=fun (type b) (witness: b witness) : (Ptime.t, b) eq option ->
                        match witness with
                        | DATE -> Some Refl
                        | _ -> None}}

  type 'a witness += DOUBLE_PRECISION : float witness
  let double_precision = CUSTOM { ty = Caqti_type.float; repr = "DOUBLE PRECISION"; witness=DOUBLE_PRECISION;
                                  eq_witness={eq=fun (type b) (witness: b witness) : (float, b) eq option ->
                                    match witness with
                                    | DOUBLE_PRECISION -> Some Refl
                                    | _ -> None}}

  type 'a witness += INT4 : int32 witness
  let int4 = CUSTOM { ty = Caqti_type.int32; repr = "INT4"; witness=INT4;
                      eq_witness={eq=fun (type b) (witness: b witness) : (int32, b) eq option ->
                        match witness with
                        | INT4 -> Some Refl
                        | _ -> None}}

  type 'a witness += SMALLINT : int witness
  let smallint = CUSTOM { ty = Caqti_type.int16; repr = "SMALLINT" ; witness=SMALLINT;
                          eq_witness={eq=fun (type b) (witness: b witness) : (int, b) eq option ->
                            match witness with
                            | SMALLINT -> Some Refl
                            | _ -> None}}

  type 'a witness += SMALLSERIAL : int witness
  let smallserial = CUSTOM { ty = Caqti_type.int16; repr = "SMALLSERIAL";witness=SMALLSERIAL;
                             eq_witness={eq=fun (type b) (witness: b witness) : (int, b) eq option ->
                               match witness with
                               | SMALLSERIAL -> Some Refl
                               | _ -> None}}

  type 'a witness += TIME : Ptime.t witness
  let time = CUSTOM { ty = Caqti_type.Std.ptime; repr = "TIME"; witness=TIME;
                      eq_witness={eq=fun (type b) (witness: b witness) : (Ptime.t, b) eq option ->
                        match witness with
                        | TIME -> Some Refl
                        | _ -> None}}

end

module Sqlite3 = struct

  type 'a witness += BLOB: string witness
  let blob = CUSTOM { ty = Caqti_type.string; repr = "BLOB"; witness=BLOB;
                      eq_witness={eq=fun (type b) (witness: b witness) : (string, b) eq option ->
                        match witness with
                        | BLOB -> Some Refl
                        | _ -> None}}

end

let int = INTEGER
let real = REAL
let text = TEXT
let bool = BOOLEAN

let null_ty : 'a . 'a t -> 'a option t =
  fun (type a) (ty: a t) : a option t ->
  match ty with
  | INTEGER -> NULLABLE INTEGER
  | REAL -> NULLABLE REAL
  | TEXT -> NULLABLE TEXT
  | BOOLEAN -> NULLABLE BOOLEAN
  | CUSTOM def -> NULLABLE (CUSTOM def)
  | NULLABLE _ -> invalid_arg "already a nullable type"

let rec ty_to_caqti_ty: 'a . 'a t -> 'a Caqti_type.t =
  fun (type a) (ty: a t) : a Caqti_type.t ->
  match ty with
  | INTEGER -> Caqti_type.int
  | BOOLEAN -> Caqti_type.bool
  | REAL -> Caqti_type.float
  | TEXT -> Caqti_type.string
  | CUSTOM {ty;_} -> ty
  | NULLABLE ty -> Caqti_type.option (ty_to_caqti_ty ty)

let rec ty_list_to_caqti_ty: 'a . 'a ty_list -> 'a Caqti_type.t =
  fun (type a) (ls: a ty_list) : a Caqti_type.t ->
  match ls with
  | Nil -> Caqti_type.unit
  | Cons (h, t) ->
    Caqti_type.tup2 (ty_to_caqti_ty h) (ty_list_to_caqti_ty t)

let rec eq_ty: 'a 'b . 'a t * 'b t -> ('a,'b) eq option =
  fun (type a b) ((e1,e2): a t * b t) : (a,b) eq option ->
  match e1,e2 with
  | (INTEGER, INTEGER) -> Some Refl
  | (REAL, REAL) -> Some Refl
  | (TEXT, TEXT) -> Some Refl
  | (CUSTOM {ty=_; repr=_;eq_witness;_}, CUSTOM {ty=_; repr=_; witness;_}) ->
    eq_witness.eq witness
  | (NULLABLE l, NULLABLE r) -> begin match eq_ty (l,r) with
    | Some Refl -> Some Refl
    | None -> None
  end
  | _ -> None

let rec eq_ty_list : 'a 'b . 'a ty_list * 'b ty_list -> ('a,'b) eq option =
  fun (type a b) ((l1,l2): (a ty_list * b ty_list)) : (a,b) eq option ->
  match l1,l2 with
  | Nil, Nil -> Some Refl
  | Cons (h1,t1), Cons (h2, t2) -> begin
      match eq_ty (h1,h2) with
      | Some Refl -> begin match eq_ty_list (t1, t2) with
        | Some Refl -> Some Refl
        | None -> None
      end
      | None -> None
    end
  | _ -> None

let rec show : 'a . 'a t -> string =  fun (type a) (ty: a t) ->
  match ty with
  | INTEGER -> "INTEGER"
  | REAL -> "REAL"
  | BOOLEAN -> "BOOLEAN"
  | TEXT -> "TEXT"
  | CUSTOM {repr;_} -> repr
  | NULLABLE ty -> show ty

let rec pp_value : 'a . 'a t -> Format.formatter -> 'a -> unit =  fun (type a) (ty: a t) fmt (vl: a) ->
  match ty with
  | INTEGER -> Format.fprintf fmt "%d" vl
  | REAL -> Format.fprintf fmt "%f" vl
  | BOOLEAN -> Format.fprintf fmt "%b" vl
  | TEXT -> Format.fprintf fmt "%s" (Caqti_sql.sql_escaped vl)[@alert "-deprecated"]
  | CUSTOM {ty;_} -> Caqti_type.pp_value fmt (ty,vl)
  | NULLABLE ty ->
    match vl with
    | None -> Format.fprintf fmt "NULL"
    | Some vl -> pp_value ty fmt vl

module Map (S: sig type ('key,'vl) t end) : sig
  type !'a t
  val empty: 'a t

  val lookup_opt : 'opt t -> key:'k ty_list -> ('k, 'opt) S.t option

  val insert : 'a t -> key:'k ty_list -> data:('k, 'a) S.t -> 'a t
end = struct

  type !'a t =
    | [] : 'a t
    | (::) : ('k ty_list * ('k, 'a) S.t) * 'a t -> 'a t

  let empty : 'a t = []

  let rec lookup_opt: 'a t -> key:'k ty_list -> ('k,'a) S.t option =
    let handle: 'a 'b . 'b ty_list -> 'a ty_list * ('a, 'opt) S.t -> ('b, 'opt) S.t option =
      fun (type a b) (l1: b ty_list) ((l2,data): (a ty_list * (a, 'opt) S.t)) : (b, 'opt) S.t option ->
        match eq_ty_list (l1, l2) with
        | Some Refl -> Some data
        | _ -> None in
    fun ls ~key ->
      match ls with
      | [] -> None
      | data :: t ->
        match handle key data with
        | Some data -> Some data
        | None -> lookup_opt t ~key

  let insert : 'a t -> key:'k ty_list -> data:('k,'a) S.t -> 'a t =
    fun ls ~key ~data -> (key,data) :: ls
end

