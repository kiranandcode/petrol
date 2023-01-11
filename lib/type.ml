type (_,_) eq = Refl: ('a,'a) eq

type 'a t =
  | BOOL : bool t
  | INTEGER : int t
  | REAL: float t
  | TEXT : string t
  | BLOB : string t

  | NULLABLE_BOOL: bool option t
  | NULLABLE_INTEGER: int option t
  | NULLABLE_REAL: float option t
  | NULLABLE_TEXT: string option t
  | NULLABLE_BLOB: string option t

type _ ty_list =
  | Nil : unit ty_list
  | Cons : 'a t * 'b ty_list -> ('a * 'b) ty_list


let bool = BOOL
let int = INTEGER
let real = REAL
let text = TEXT
let blob = BLOB

let null_ty: 'a . 'a t -> 'a option t =
  fun (type a) (ty: a t) : a option t ->
  match ty with
  | BOOL      -> NULLABLE_BOOL   
  | INTEGER   -> NULLABLE_INTEGER
  | REAL      -> NULLABLE_REAL   
  | TEXT      -> NULLABLE_TEXT   
  | BLOB      -> NULLABLE_BLOB   
  | _ -> invalid_arg "can not null a nullable type"

let ty_to_caqti_ty: 'a . 'a t -> 'a Caqti_type.t =
  fun (type a) (ty: a t) : a Caqti_type.t ->
  match ty with
  | BOOL -> Caqti_type.bool
  | INTEGER -> Caqti_type.int
  | REAL -> Caqti_type.float
  | TEXT -> Caqti_type.string
  | BLOB -> Caqti_type.string

  | NULLABLE_BOOL -> Caqti_type.option Caqti_type.bool
  | NULLABLE_INTEGER -> Caqti_type.option Caqti_type.int
  | NULLABLE_REAL -> Caqti_type.option Caqti_type.float
  | NULLABLE_TEXT -> Caqti_type.option Caqti_type.string
  | NULLABLE_BLOB -> Caqti_type.option Caqti_type.string

let rec ty_list_to_caqti_ty: 'a . 'a ty_list -> 'a Caqti_type.t =
  fun (type a) (ls: a ty_list) : a Caqti_type.t ->
  match ls with
  | Nil -> Caqti_type.unit
  | Cons (h, t) ->
    Caqti_type.tup2 (ty_to_caqti_ty h) (ty_list_to_caqti_ty t)

let eq_ty: 'a 'b . 'a t * 'b t -> ('a,'b) eq option =
  fun (type a b) ((e1,e2): a t * b t) : (a,b) eq option ->
  match e1,e2 with
  | (BOOL, BOOL) -> Some Refl
  | (INTEGER, INTEGER) -> Some Refl
  | (REAL, REAL) -> Some Refl
  | (TEXT, TEXT) -> Some Refl
  | (BLOB, BLOB) -> Some Refl
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

let show : 'a . 'a t -> string =  fun (type a) (ty: a t) ->
  match ty with
  | BOOL | INTEGER -> "INTEGER"
  | REAL -> "REAL"
  | TEXT -> "TEXT"
  | BLOB -> "BLOB"

  | NULLABLE_BOOL | NULLABLE_INTEGER -> "INTEGER"
  | NULLABLE_REAL -> "REAL"
  | NULLABLE_TEXT -> "TEXT"
  | NULLABLE_BLOB -> "BLOB"

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

