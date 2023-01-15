type 'a t = 'a Types.expr

type 'a expr_list = 'a Types.expr_list =
  | [] : unit expr_list
  | (::) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

type wrapped_assign = Types.wrapped_assign

type wrapped_value =
  Types.wrapped_value = MkWrapped: 'a Type.t * 'a -> wrapped_value

let pp_wrapped_value fmt = function
  | MkWrapped (BOOL, b) -> Format.fprintf fmt "(BOOL,%b)" b
  | MkWrapped (INTEGER, i) -> Format.fprintf fmt "(INTEGER,%d)" i
  | MkWrapped (REAL, f) -> Format.fprintf fmt "(REAL,%f)" f
  | MkWrapped (TEXT, t) -> Format.fprintf fmt "(TEXT,'%s')" t
  | MkWrapped (BLOB, t) -> Format.fprintf fmt "(BLOB,'%s')" t

  | MkWrapped (NULLABLE_BOOL, None) -> Format.fprintf fmt "(BOOL,NULL)"
  | MkWrapped (NULLABLE_BOOL, Some b) -> Format.fprintf fmt "(BOOL,%b)" b
  | MkWrapped (NULLABLE_INTEGER, None) -> Format.fprintf fmt "(INTEGER,NULL)"
  | MkWrapped (NULLABLE_INTEGER, Some i) -> Format.fprintf fmt "(INTEGER,%d)" i
  | MkWrapped (NULLABLE_REAL, None) -> Format.fprintf fmt "(REAL,NULL)"
  | MkWrapped (NULLABLE_REAL, Some f) -> Format.fprintf fmt "(REAL,%f)" f
  | MkWrapped (NULLABLE_TEXT, None) -> Format.fprintf fmt "(TEXT,'NULL)"
  | MkWrapped (NULLABLE_TEXT, Some t) -> Format.fprintf fmt "(TEXT,'%s')" t
  | MkWrapped (NULLABLE_BLOB, None) -> Format.fprintf fmt "(BLOB,'NULL)"
  | MkWrapped (NULLABLE_BLOB, Some t) -> Format.fprintf fmt "(BLOB,'%s')" t

[@@warning "-32"]

let pp = Types.pp_expr
let pp_expr_list = Types.pp_expr_list

let values = Types.values_expr
let values_expr_list = Types.values_expr_list

let rec ty: 'a . 'a t -> 'a Type.t = fun (type a) (expr: a t) : a Type.t ->
  match expr with
  | NULL ty -> ty
  | SUB (_, _) -> INTEGER
  | ADD (_, _) -> INTEGER
  | NOT _ -> BOOL
  | EXISTS _ -> BOOL
  | AND (_, _) -> BOOL
  | OR (_, _) -> BOOL
  | COMPARE (_, _, _) -> BOOL
  | IS_NOT_NULL _ -> BOOL
  | FIELD (_, _, ty) -> ty
  | CONST (_, ty) -> ty
  | CONST_STATIC (_, ty) -> ty
  | COERCETO (_, ty) -> ty
  | AS (expr, _) -> ty expr
  | REF (_, ty) -> ty
  | COUNT (_, _) -> INTEGER
  | COUNT_STAR -> INTEGER
  | MAX (_, _) -> INTEGER
  | MIN (_, _) -> INTEGER
  | SUM (_, _) -> INTEGER
  | TOTAL (_, _) -> INTEGER
  | GROUP_CONCAT (_, _, _) -> TEXT
  | ABS _ -> INTEGER
  | CHANGES -> INTEGER
  | GLOB (_, _) -> BOOL
  | COALESCE [] -> failwith "empty coalesce not supported"
  | COALESCE (h::_) -> ty h
  | LIKE (_, _) -> BOOL
  | MIN_OF _ -> INTEGER
  | MAX_OF _ -> INTEGER
  | RANDOM -> INTEGER
  | LOWER _ -> TEXT
  | UPPER _ -> TEXT
  | NULLABLE expr -> Type.null_ty (ty expr)

let rec ty_expr_list : 'a . 'a expr_list -> 'a Type.ty_list =
  fun (type a) (ls: a expr_list) : a Type.ty_list ->
  match ls with
  | [] -> Nil
  | h :: t ->
    Cons (ty h, ty_expr_list t)

let i i = Types.CONST (i,INTEGER)
let f i = Types.CONST (i,REAL)
let s i = Types.CONST (i,TEXT)
let b i = Types.CONST (i,BLOB)
let bl i = Types.CONST (i,BOOL)

let i_stat i = Types.CONST_STATIC (i,INTEGER)
let f_stat i = Types.CONST_STATIC (i,REAL)
let s_stat i = Types.CONST_STATIC (i,TEXT)
let b_stat i = Types.CONST_STATIC (i,BLOB)

let nullable v = Types.NULLABLE v

let true_ = Types.CONST_STATIC (true,BOOL)
let false_ = Types.CONST_STATIC (false,BOOL)

let (:=) l r =
  match l with
  | Types.FIELD fld -> Types.ASSIGN (fld,r)
  | _ -> invalid_arg "LHS of an assignment must be a field"

let null : 'a. 'a Type.t -> 'a option Type.t =
  fun (type a) (ty: a Type.t) : a option Type.t ->
  match ty with
  | Type.BOOL -> NULLABLE_BOOL
  | Type.INTEGER -> NULLABLE_INTEGER
  | Type.REAL -> NULLABLE_REAL
  | Type.TEXT -> NULLABLE_TEXT
  | Type.BLOB -> NULLABLE_BLOB
  | Type.NULLABLE_BOOL
  | Type.NULLABLE_INTEGER
  | Type.NULLABLE_REAL
  | Type.NULLABLE_TEXT
  | Type.NULLABLE_BLOB ->
    failwith "invalid assumptions"

let unset l =
  match l with
  | Types.FIELD (tbl, fld, ty) ->
    Types.ASSIGN ((tbl, fld, (null ty)), Types.NULL (null ty))
  | _ -> invalid_arg "LHS of an unset must be a field"

let (+) l r = Types.ADD (l, r)
let (-) l r = Types.SUB (l, r)

let (=) l r = Types.COMPARE (EQ, l, r)
let (<>) l r = Types.COMPARE (NEQ, l, r)
let (<=) l r = Types.COMPARE (LE, l, r)
let (<) l r = Types.COMPARE (LT, l, r)
let (>) l r = Types.COMPARE (GT, l, r)
let (>=) l r = Types.COMPARE (GE, l, r)

let (&&) l r = Types.AND (l, r)
let (||) l r = Types.OR (l, r)
let not cond = Types.NOT cond
let exists q = Types.EXISTS q

let is_not_null expr = Types.IS_NOT_NULL expr
let coerce expr ty = Types.COERCETO (expr,ty)
let as_ expr ~name = Types.AS (expr,name), Types.REF (name, ty expr)

let count ?(distinct=false) exprs = Types.COUNT (distinct,exprs)
let count_star = Types.COUNT_STAR
let max ?(distinct=false) expr = Types.MAX (distinct,expr)
let min ?(distinct=false) expr = Types.MIN (distinct,expr)
let sum ?(distinct=false) expr = Types.SUM (distinct,expr)
let total ?(distinct=false) expr = Types.TOTAL (distinct,expr)
let group_concat ?(distinct=false) ?sep_by l = Types.GROUP_CONCAT (distinct, l, sep_by)


let abs expr = Types.ABS(expr)
let changes = Types.CHANGES
let glob ~pat:x y = Types.GLOB (x,y)
let coalesce exprs = Types.COALESCE exprs
let like x ~pat:y = Types.LIKE (x,y)

let max_of exprs = Types.MAX_OF exprs
let min_of exprs = Types.MIN_OF exprs

let random = Types.RANDOM
let lower s = Types.LOWER s
let upper s = Types.UPPER s

