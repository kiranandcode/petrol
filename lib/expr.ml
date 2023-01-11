type 'a t = 'a Types.expr

type 'a expr_list = 'a Types.expr_list =
  | [] : unit expr_list
  | (::) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

type wrapped_value = MkWrapped: 'a Type.t * 'a -> wrapped_value

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

let rec values : 'a . wrapped_value list -> 'a t
  -> wrapped_value list =
  fun acc (type a) (expr: a t) ->
  match expr with
  | NULLABLE expr -> values acc expr
  | COMPARE (_, l, r) ->
    values (values acc l) r
  | SUB (l, r)
  | ADD (l, r) ->
    values (values acc l) r
  | OR (l, r)
  | AND (l, r) ->
    values (values acc l) r
  | IS_NOT_NULL expr ->
    values acc expr
  | FIELD _ -> acc
  | CONST (vl, ty) ->
    (MkWrapped (ty,vl)) :: acc
  | CONST_STATIC (_, _) -> acc
  | COERCETO (expr, _) -> values acc expr
  | AS (expr, _) -> values acc expr
  | REF _ -> acc
  | COUNT (_, exprs) -> values_expr_list acc exprs
  | COUNT_STAR -> acc
  | GROUP_CONCAT (_, l, None) ->
    values acc l
  | GROUP_CONCAT (_, l, Some r) ->
    values (values acc l) r
  | MAX (_, expr) -> values acc expr
  | MIN (_, expr) -> values acc expr
  | SUM (_, expr) -> values acc expr
  | TOTAL (_, expr) -> values acc expr
  | ABS expr -> values acc expr
  | CHANGES -> acc
  | GLOB (l, r) -> values (values acc l) r
  | COALESCE exprs ->
    List.fold_left values acc exprs
  | LIKE (l, r) -> values (values acc l) r
  | MIN_OF exprs ->
    List.fold_left values acc exprs
  | MAX_OF exprs ->
    List.fold_left values acc exprs
  | RANDOM -> acc
  | LOWER expr
  | UPPER expr -> values acc expr
and values_expr_list :
  'a . wrapped_value list -> 'a expr_list -> wrapped_value list =
  fun acc (type a) (exprs: a expr_list) ->
  match exprs with
  | [] -> acc
  | h :: t -> values_expr_list (values acc h) t

let rec pp  : 'a . Format.formatter -> 'a t -> unit =
  fun fmt (type a) (expr: a t) ->
  match expr with
  | NULLABLE expr -> pp fmt expr
  | COMPARE (op, l, r) ->
    let op = match op with
      | EQ -> "="
      | NEQ -> "!="
      | GT -> ">"
      | GE -> ">="
      | LT -> "<"
      | LE -> "<=" in
    Format.fprintf fmt "%a %s %a"
      pp l op pp r
  | ADD (l, r) ->
    Format.fprintf fmt "(%a) + (%a)"
      pp l pp r
  | SUB (l, r) ->
    Format.fprintf fmt "(%a) - (%a)"
      pp l pp r
  | AND (l,r) ->
    Format.fprintf fmt "%a AND %a"
      pp l pp r
  | OR (l, r) ->
    Format.fprintf fmt "%a AND %a"
      pp l pp r
  | IS_NOT_NULL expr -> 
    Format.fprintf fmt "%a IS NOT NULL"
      pp expr
  | FIELD (table_name, field_name, _ty) ->
    let table_name = snd table_name in
    Format.fprintf fmt "%s.%s" table_name field_name
  | CONST_STATIC (vl, ty) -> begin match (ty: a Type.t), vl with
    | BOOL, true -> Format.fprintf fmt "TRUE"
    | BOOL, false -> Format.fprintf fmt "FALSE"
    | (INTEGER, i) -> Format.fprintf fmt "%d" i
    | (REAL, f) -> Format.fprintf fmt "%f" f
    | (TEXT, s) -> Format.fprintf fmt "'%s'" s
    | (BLOB, s) -> Format.fprintf fmt "'%s'" s

    | (NULLABLE_BOOL, None) -> Format.fprintf fmt "NULL"
    | (NULLABLE_BOOL, Some true) -> Format.fprintf fmt "TRUE"
    | (NULLABLE_BOOL, Some false) -> Format.fprintf fmt "FALSE"

    | (NULLABLE_INTEGER, None) -> Format.fprintf fmt "NULL"
    | (NULLABLE_INTEGER, Some i) -> Format.fprintf fmt "%d" i
    | (NULLABLE_REAL, None) -> Format.fprintf fmt "NULL"
    | (NULLABLE_REAL, Some f) -> Format.fprintf fmt "%f" f
    | (NULLABLE_TEXT, None) -> Format.fprintf fmt "NULL"
    | (NULLABLE_TEXT, Some s) -> Format.fprintf fmt "'%s'" s
    | (NULLABLE_BLOB, None) -> Format.fprintf fmt "NULL"
    | (NULLABLE_BLOB, Some s) -> Format.fprintf fmt "'%s'" s

  end
  | CONST (_, _) -> Format.fprintf fmt "?"
  | COERCETO (expr, _) -> pp fmt expr
  | AS (expr, name) ->
    Format.fprintf fmt "%a AS %s" pp expr name
  | REF (name,_) ->
    Format.fprintf fmt "%s" name
  | COUNT (false, exprs) ->
    Format.fprintf fmt "COUNT(%a)" pp_expr_list exprs
  | COUNT (true, exprs) -> 
    Format.fprintf fmt "COUNT(DISTINCT %a)" pp_expr_list exprs
  | COUNT_STAR ->
    Format.fprintf fmt "COUNT(*)"
  | MAX (false, expr) -> 
    Format.fprintf fmt "MAX(%a)" pp expr
  | MAX (true, expr) -> 
    Format.fprintf fmt "MAX(DISTINCT %a)" pp expr
  | MIN (false, expr) -> 
    Format.fprintf fmt "MIN(%a)" pp expr
  | MIN (true, expr) -> 
    Format.fprintf fmt "MIN(DISTINCT %a)" pp expr
  | SUM (false, expr) -> 
    Format.fprintf fmt "SUM(%a)" pp expr
  | SUM (true, expr) -> 
    Format.fprintf fmt "SUM(DISTINCT %a)" pp expr
  | TOTAL (false, expr) -> 
    Format.fprintf fmt "TOTAL(%a)" pp expr
  | TOTAL (true, expr) -> 
    Format.fprintf fmt "TOTAL(DISTINCT %a)" pp expr
  | GROUP_CONCAT (false, l, None) ->
    Format.fprintf fmt "GROUP_CONCAT(%a)" pp l
  | GROUP_CONCAT (false, l, Some r) ->
    Format.fprintf fmt "GROUP_CONCAT(%a, %a)" pp l pp r
  | GROUP_CONCAT (true, l, None) ->
    Format.fprintf fmt "GROUP_CONCAT(DISTINCT %a)" pp l
  | GROUP_CONCAT (true, l, Some r) ->
    Format.fprintf fmt "GROUP_CONCAT(DISTINCT %a, %a)" pp l pp r
  | ABS expr ->
    Format.fprintf fmt "ABS(%a)" pp expr
  | CHANGES -> Format.fprintf fmt "CHANGES()"
  | GLOB (pat, expr) ->
    Format.fprintf fmt "GLOB(%a, %a)" pp pat pp expr
  | COALESCE exprs ->
    Format.fprintf fmt "COALESCE(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () ->
         Format.fprintf fmt ", ") pp) exprs
  | LIKE (pat, s) ->
    Format.fprintf fmt "LIKE(%a,%a)" pp pat pp s
  | MIN_OF exprs ->
    Format.fprintf fmt "MIN(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () ->
         Format.fprintf fmt ", ") pp) exprs
  | MAX_OF exprs -> 
    Format.fprintf fmt "MAX(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () ->
         Format.fprintf fmt ", ") pp) exprs
  | RANDOM ->
    Format.fprintf fmt "RANDOM()"
  | LOWER s ->
    Format.fprintf fmt "LOWER(%a)" pp s
  | UPPER s ->
    Format.fprintf fmt "UPPER(%a)" pp s
and pp_expr_list_inner : 'a . Format.formatter -> 'a expr_list -> unit =
  fun fmt (type a) (ls: a expr_list) -> match ls with
    | [] -> ()
    | h :: t -> Format.fprintf fmt ", %a%a"
                  pp h pp_expr_list_inner t
and pp_expr_list : 'a . Format.formatter -> 'a expr_list -> unit =
  fun fmt (type a) (ls: a expr_list) -> match ls with
    | [] -> ()
    | h :: t -> Format.fprintf fmt "%a%a"
                  pp h pp_expr_list_inner t

let rec ty: 'a . 'a t -> 'a Type.t = fun (type a) (expr: a t) : a Type.t ->
  match expr with
  | SUB (_, _) -> INTEGER
  | ADD (_, _) -> INTEGER
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

