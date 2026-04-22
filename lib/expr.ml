type 'a t = 'a Types.expr

type 'a expr_list = 'a Types.expr_list =
  | [] : unit expr_list
  | (::) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

type wrapped_assign = Types.wrapped_assign

type wrapped_value =
  Types.wrapped_value = MkWrapped: 'a Type.t * 'a -> wrapped_value

let pp_wrapped_value fmt (MkWrapped (ty, vl)) =
  Format.fprintf fmt "(%s,%a)" (Type.show ty) (Type.pp_value ty) vl

let pp = Types.pp_expr
let pp_expr_list = Types.pp_expr_list

let values = Types.values_expr
let values_expr_list = Types.values_expr_list

let ty_expr = Types.ty_expr
let ty_expr_list = Types.ty_expr_list

module Common = struct

  let i i = Types.CONST (i,INTEGER)
  let f i = Types.CONST (i,REAL)
  let s i = Types.CONST (i,TEXT)
  let bl i = Types.CONST (i,Type.bool)
  let vl ~ty i = Types.CONST (i,ty)

  let vl_opt ~ty i = Types.CONST (i, NULLABLE ty)
  let i_opt i = Types.CONST (i, NULLABLE INTEGER)
  let f_opt i = Types.CONST (i, NULLABLE REAL)
  let s_opt i = Types.CONST (i, NULLABLE TEXT)
  let bl_opt i = Types.CONST (i, NULLABLE Type.bool)

  let i_stat i = Types.CONST_STATIC (i,INTEGER)
  let f_stat i = Types.CONST_STATIC (i,REAL)
  let s_stat i = Types.CONST_STATIC (i,TEXT)
  let true_ = Types.CONST_STATIC (true,Type.bool)
  let false_ = Types.CONST_STATIC (false,Type.bool)

  let vl_stat ~ty i = Types.CONST_STATIC (i,ty)

  let as_ expr ~name = Types.AS (expr,name), Types.REF (name, Types.ty_expr expr)


  let nullable v = Types.Common.NULLABLE v

  let (:=) l r =
    match l with
    | Types.FIELD fld -> Types.ASSIGN (fld,r)
    | _ -> invalid_arg "LHS of an assignment must be a field"

  let null : 'a. 'a Type.t -> 'a option Type.t =
    fun (type a) (ty: a Type.t) : a option Type.t ->
    Type.null_ty ty

  let unset l =
    match l with
    | Types.FIELD (tbl, fld, ty) ->
      Types.ASSIGN ((tbl, fld, (null ty)), Types.NULL (null ty))
    | _ -> invalid_arg "LHS of an unset must be a field"

  let (+) l r = Types.Common.ADD (Type.Numeric.Int, l, r)
  let (-) l r = Types.Common.SUB (Type.Numeric.Int, l, r)
  let (/) l r = Types.Common.DIV (Type.Numeric.Int, l, r)
  let ( * ) l r = Types.Common.MUL (Type.Numeric.Int, l, r)

  let (+.) l r = Types.Common.ADD (Type.Numeric.Real, l, r)
  let (-.) l r = Types.Common.SUB (Type.Numeric.Real, l, r)
  let (/.) l r = Types.Common.DIV (Type.Numeric.Real, l, r)
  let ( *. ) l r = Types.Common.MUL (Type.Numeric.Real, l, r)

  let (=) l r = Types.Common.COMPARE (EQ, l, r)
  let (<>) l r = Types.Common.COMPARE (NEQ, l, r)
  let (<=) l r = Types.Common.COMPARE (LE, l, r)
  let (<) l r = Types.Common.COMPARE (LT, l, r)
  let (>) l r = Types.Common.COMPARE (GT, l, r)
  let (>=) l r = Types.Common.COMPARE (GE, l, r)

  let (&&) l r = Types.Common.AND (l, r)
  let (||) l r = Types.Common.OR (l, r)
  let not cond = Types.Common.NOT cond
  let exists q = Types.Common.EXISTS q
  let in_ expr query = Types.Common.IN (expr,query)

  let between ~lower ~upper x = Types.Common.BETWEEN (x,lower,upper)
  let not_between ~lower ~upper x = Types.Common.NOT_BETWEEN (x,lower,upper)

  let is_not_null expr = Types.Common.IS_NOT_NULL expr
  let is_null expr = Types.Common.IS_NULL expr
  let coerce expr ty = Types.COERCETO (expr,ty)

  let count ?(distinct=false) exprs = Types.Common.COUNT (distinct,exprs)
  let count_star = Types.Common.COUNT_STAR
  let max ?(distinct=false) expr = Types.Common.MAX (distinct,Type.Numeric.Int,expr)
  let min ?(distinct=false) expr = Types.Common.MIN (distinct,Type.Numeric.Int,expr)
  let sum ?(distinct=false) expr = Types.Common.SUM (distinct,Type.Numeric.Int,expr)

  let maxf ?(distinct=false) expr = Types.Common.MAX (distinct,Type.Numeric.Real,expr)
  let minf ?(distinct=false) expr = Types.Common.MIN (distinct,Type.Numeric.Real,expr)
  let sumf ?(distinct=false) expr = Types.Common.SUM (distinct,Type.Numeric.Real,expr)

  let max_gen ?(distinct=false) ty expr = Types.Common.MAX (distinct,ty,expr)
  let min_gen ?(distinct=false) ty expr = Types.Common.MIN (distinct,ty,expr)
  let sum_gen ?(distinct=false) ty expr = Types.Common.SUM (distinct,ty,expr)

  let abs expr = Types.Common.ABS (Type.Numeric.Int, expr)
  let absf expr = Types.Common.ABS (Type.Numeric.Real, expr)
  let abs_gen ty expr = Types.Common.ABS (ty, expr)

  let coalesce exprs = Types.Common.COALESCE exprs
  let like x ~pat:y = Types.Common.LIKE (y,x)

  let lower s = Types.Common.LOWER s
  let upper s = Types.Common.UPPER s

end

module Postgres = struct
  include Common

  let between_symmetric ~lower ~upper x = Types.Postgres.BETWEEN_SYMMETRIC (x,lower,upper)
  let not_between_symmetric ~lower ~upper x = Types.Postgres.NOT_BETWEEN_SYMMETRIC (x,lower,upper)

  let is_distinct_from x y = Types.Postgres.IS_DISTINCT_FROM (x,y)
  let is_not_distinct_from x y = Types.Postgres.IS_NOT_DISTINCT_FROM (x,y)

  let is_true x = Types.Postgres.IS_TRUE x
  let is_not_true x = Types.Postgres.IS_NOT_TRUE x

  let is_false x = Types.Postgres.IS_FALSE x
  let is_not_false x = Types.Postgres.IS_NOT_FALSE x

  let is_unknown x = Types.Postgres.IS_UNKNOWN x
  let is_not_unknown x = Types.Postgres.IS_NOT_UNKNOWN x

  let ceil x = Types.Postgres.CEIL (Type.Numeric.Real, x)
  let floor x = Types.Postgres.FLOOR (Type.Numeric.Real, x)
  let round x = Types.Postgres.ROUND (Type.Numeric.Real, x)
  let trunc  x = Types.Postgres.TRUNC (Type.Numeric.Real, x)

  let ceili x = Types.Postgres.CEIL (Type.Numeric.Int, x)
  let floori x = Types.Postgres.FLOOR (Type.Numeric.Int, x)
  let roundi x = Types.Postgres.ROUND (Type.Numeric.Int, x)
  let trunci  x = Types.Postgres.TRUNC (Type.Numeric.Int, x)

  let ceil_gen ~ty x = Types.Postgres.CEIL (ty, x)
  let floor_gen ~ty x = Types.Postgres.FLOOR (ty, x)
  let round_gen ~ty x = Types.Postgres.ROUND (ty, x)
  let trunc_gen ~ty  x = Types.Postgres.TRUNC (ty, x)

  let pi = Types.Postgres.PI
  let sqrt  x = Types.Postgres.SQRT x
  let degrees  x = Types.Postgres.DEGREES x
  let radians  x = Types.Postgres.RADIANS x
  let exp  x = Types.Postgres.EXP x
  let ln  x = Types.Postgres.LN x
  let log10  x = Types.Postgres.LOG10 x
  let log ~base x = Types.Postgres.LOG (base, x)
  let power x y = Types.Postgres.POWER (Type.Numeric.Real, x, y) (* x ^ y *)
  let poweri x y = Types.Postgres.POWER (Type.Numeric.Int, x, y) (* x ^ y *)
  let power_gen ~ty x y = Types.Postgres.POWER (ty, x, y) (* x ^ y *)


  let cos  x = Types.Postgres.COS x
  let cosd  x = Types.Postgres.COSD x
  let acos  x = Types.Postgres.ACOS x
  let acosd  x = Types.Postgres.ACOSD x
  let cosh  x = Types.Postgres.COSH x
  let acosh  x = Types.Postgres.ACOSH x

  let sin  x = Types.Postgres.SIN x
  let sind  x = Types.Postgres.SIND x
  let asin  x = Types.Postgres.ASIN x
  let asind  x = Types.Postgres.ASIND x
  let sinh  x = Types.Postgres.SINH x
  let asinh  x = Types.Postgres.ASINH x


  let tan  x = Types.Postgres.TAN x
  let tand  x = Types.Postgres.TAND x
  let atan  x = Types.Postgres.ATAN x
  let atand  x = Types.Postgres.ATAND x
  let atan2  x = Types.Postgres.ATAN2 x
  let atan2d  x = Types.Postgres.ATAN2D x
  let tanh  x = Types.Postgres.TANH x
  let atanh  x = Types.Postgres.ATANH x

  let cot  x = Types.Postgres.COT x
  let cotd  x = Types.Postgres.COTD x

  let factorial  x = Types.Postgres.FACTORIAL (Type.Numeric.Int, x)
  let factorial_gen ~ty  x = Types.Postgres.FACTORIAL (ty, x)

  let gcd x y = Types.Postgres.GCD (Type.Numeric.Int, x, y)
  let gcd_gen ~ty x y = Types.Postgres.GCD (ty, x, y)
  let lcm x y = Types.Postgres.LCM (Type.Numeric.Int, x,y)
  let lcm_gen ~ty x y = Types.Postgres.LCM (ty, x,y)
  let concat x = Types.Postgres.CONCAT x
  let concat_ws ~sep_by x = Types.Postgres.CONCAT_WS (sep_by,x)
  let char_length x = Types.Postgres.CHAR_LENGTH x
  let length  x = Types.Postgres.LENGTH x

  let substring ?from ?for_ txt = Types.Postgres.SUBSTRING (txt, from, for_)
  (* first int is from, second int is for *)
  let replace ~from ~to_ x = Types.Postgres.REPLACE (x, from, to_)
  (* args (string,from,to) *)
  let reverse x = Types.Postgres.REVERSE x
  let starts_with ~prefix x = Types.Postgres.STARTS_WITH (x, prefix)

  let similar_to ~pat x = Types.Postgres.SIMILAR_TO (x, pat)
  (* args(string, pattern) *)

  let greatest x = Types.Postgres.GREATEST (Type.Numeric.Int, x)
  let greatestf x = Types.Postgres.GREATEST (Type.Numeric.Real, x)
  let greatest_gen ~ty x = Types.Postgres.GREATEST (ty, x)
  let least  x = Types.Postgres.LEAST (Type.Numeric.Int, x)
  let leastf  x = Types.Postgres.LEAST (Type.Numeric.Real, x)
  let least_gen ~ty  x = Types.Postgres.LEAST (ty, x)

  let random = Types.Postgres.RANDOM

end

module Sqlite3 = struct
  include Common

  let b i = Types.CONST (i,Type.Sqlite3.blob)
  let b_stat i = Types.CONST_STATIC (i,Type.Sqlite3.blob)

  let total ?(distinct=false) expr = Types.Sqlite3.TOTAL (distinct,expr)
  let group_concat ?(distinct=false) ?sep_by l = Types.Sqlite3.GROUP_CONCAT (distinct, l, sep_by)

  let changes = Types.Sqlite3.CHANGES

  let glob ~pat:x y = Types.Sqlite3.GLOB (x,y)

  let random = Types.Sqlite3.RANDOM

  let max_of exprs = Types.Sqlite3.MAX_OF (Type.Numeric.Int, exprs)
  let min_of exprs = Types.Sqlite3.MIN_OF (Type.Numeric.Int, exprs)

  let maxf_of exprs = Types.Sqlite3.MAX_OF (Type.Numeric.Real, exprs)
  let minf_of exprs = Types.Sqlite3.MIN_OF (Type.Numeric.Real, exprs)

  let max_gen_of ty exprs = Types.Sqlite3.MAX_OF (ty, exprs)
  let min_gen_of ty exprs = Types.Sqlite3.MIN_OF (ty, exprs)

end


