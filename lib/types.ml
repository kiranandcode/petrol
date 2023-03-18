type table_name = int * string

type comparison = EQ | NEQ
                | GT | GE
                | LT | LE

type join_op = LEFT | RIGHT | INNER 
let pp_join_op fmt = function
  | LEFT -> Format.fprintf fmt "LEFT JOIN"
  | RIGHT -> Format.fprintf fmt "RIGHT JOIN"
  | INNER -> Format.fprintf fmt "INNER JOIN"

let pp_ordering fmt = function
  | `ASC -> Format.fprintf fmt "ASC"
  | `DESC -> Format.fprintf fmt "DESC"

let pp_on_err : Format.formatter -> [ `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ] -> unit =
  fun fmt -> function
    | `IGNORE -> Format.fprintf fmt "OR IGNORE"
    | `REPLACE -> Format.fprintf fmt "OR REPLACE"
    | `ABORT -> Format.fprintf fmt "OR ABORT"
    | `FAIL -> Format.fprintf fmt "OR FAIL"
    | `ROLLBACK -> Format.fprintf fmt "OR ROLLBACK"

let pp_on_conflict : Format.formatter -> [ `DO_NOTHING ] -> unit =
  fun fmt -> function
    | `DO_NOTHING -> Format.fprintf fmt "ON CONFLICT DO NOTHING"

let pp_opt f fmt = function
  | None -> ()
  | Some vl -> Format.fprintf fmt "\n%a" f vl

type 'a field = table_name * string * 'a Type.t

type 'a expr_list =
  | [] : unit expr_list
  | (::) : ('a expr * 'b expr_list) -> ('a * 'b) expr_list

and 'a expr = .. 
and wrapped_assign = ASSIGN : 'a field * 'a expr -> wrapped_assign
and (_, !'res) query =
    | SELECT_CORE : {
      exprs: 'a expr_list;
      table: table_name;
      join: join list;
      where: bool expr option;
      group_by: 'b expr_list option;
      having: bool expr option;
    } -> ('a, [> `SELECT_CORE] as 'res) query
  | SELECT : {
      core: ('a, [< `SELECT_CORE ]) query;
      order_by: ([`ASC | `DESC] * 'e expr_list) option;
      limit: int expr option;
      offset: int expr option
    } -> ('a, [> `SELECT] as 'res) query
  | DELETE : {
    table: table_name;
    where: bool expr option;
  } -> (unit, [> `DELETE] as 'res) query
  | UPDATE : {
    table: table_name;
    on_err: [`ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ] option;
    set: wrapped_assign list;
    where: bool expr option;
  } -> (unit, [> `UPDATE] as 'res) query
  | INSERT : {
    table: table_name;
    on_err: [`ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ] option;
    on_conflict: [`DO_NOTHING] option;
    set: wrapped_assign list;
  } -> (unit, [> `INSERT] as 'res) query

and join = MkJoin: {
  table: ('r, [< `SELECT_CORE | `SELECT ]) query;
  on: bool expr;
  join_op: join_op;
} -> join

type wrapped_value = MkWrapped: 'a Type.t * 'a -> wrapped_value

type printer = { pp_expr: 'a . Format.formatter -> 'a expr -> unit } 
let pp_fn : printer ref = ref { pp_expr = fun (type a) _fmt (expr: a expr) : unit ->
  Format.ksprintf failwith "could not resolve printer for expression %s"
    (Obj.Extension_constructor.name
       (Obj.Extension_constructor.of_val expr)) }
let pp_expr fmt expr = (!pp_fn).pp_expr fmt expr
(* let () = pp_fn := { pp_expr = fun (type a) fmt (expr: a expr) : unit -> pp_expr fmt expr } *)
let add_printer f = let old_pp = !pp_fn in pp_fn := f old_pp

type collector = { values_expr : 'a . wrapped_value list -> 'a expr -> wrapped_value list }
let values_fn : collector ref = ref { values_expr = fun (type a) _ (expr: a expr) ->
  Format.ksprintf failwith "could not resolve collector for expression %s"
    (Obj.Extension_constructor.name
       (Obj.Extension_constructor.of_val expr))
}
let values_expr acc expr = (!values_fn).values_expr acc expr
let add_collector f = let old_f = !values_fn in values_fn := f old_f

type typer = { ty_expr : 'a . 'a expr -> 'a Type.t }
let typer_fn : typer ref = ref { ty_expr = fun (type a) (expr: a expr) ->
  Format.ksprintf failwith "could not resolve typer for expression %s"
    (Obj.Extension_constructor.name
       (Obj.Extension_constructor.of_val expr))
}
let ty_expr expr = (!typer_fn).ty_expr expr
let add_typer f = let old_f = !typer_fn in typer_fn := f old_f


let rec ty_expr_list : 'a . 'a expr_list -> 'a Type.ty_list =
  fun (type a) (ls: a expr_list) : a Type.ty_list ->
  match ls with
  | [] -> Nil
  | h :: t ->
    Cons (ty_expr h, ty_expr_list t)


type 'a expr +=
  | NULL : 'a option Type.t -> 'a option expr
  | CONST : 'a * 'a Type.t -> 'a expr
  | CONST_STATIC : 'a * 'a Type.t -> 'a expr
  | FIELD : 'a field -> 'a expr
  | COERCETO: 'a expr * 'b Type.t -> 'b expr
  | REF : string * 'a Type.t -> 'a expr
  | AS: 'a expr * string -> 'a expr

let () = add_printer @@ fun pp -> {
  pp_expr=fun (type a) fmt (expr: a expr) ->
    match expr with 
    | NULL _ -> Format.fprintf fmt "NULL"
    | CONST (_, _) -> Format.fprintf fmt "?"
    | CONST_STATIC (vl, ty) -> Type.pp_value ty fmt vl
    | FIELD (table_name, field_name, _) ->
      let table_name = snd table_name in
      Format.fprintf fmt "%s.%s" table_name field_name
    | COERCETO (expr, _) -> pp_expr fmt expr
    | REF (name,_) -> Format.fprintf fmt "%s" name
    | AS (expr, name) -> Format.fprintf fmt "%a AS %s" pp_expr expr name
    | _ -> pp.pp_expr fmt expr
}

let () = add_collector @@ fun collector -> {
  values_expr=fun (type a) acc (expr: a expr) : wrapped_value list ->
    match expr with 
    | NULL _ -> acc
    | CONST (vl, ty) -> (MkWrapped (ty,vl)) :: acc
    | CONST_STATIC (_, _) -> acc
    | FIELD _ -> acc
    | COERCETO (expr, _) -> values_expr acc expr
    | REF _ -> acc
    | AS (expr, _) -> values_expr acc expr
    | _ -> collector.values_expr acc expr
}

let () = add_typer @@ fun typer -> {
  ty_expr=fun (type a) (e: a expr) : a Type.t ->
    match e with
    | NULL ty -> ty
    | CONST (_, ty) -> ty
    | CONST_STATIC (_, ty) -> ty
    | FIELD (_, _, ty) -> ty
    | COERCETO (_, ty) -> ty
    | REF (_, ty) -> ty
    | AS (expr, _) -> ty_expr expr
    | _ -> typer.ty_expr e
}

let rec pp_expr_list_inner : 'a . Format.formatter -> 'a expr_list -> unit =
  fun fmt (type a) (ls: a expr_list) -> match ls with
    | [] -> ()
    | h :: t -> Format.fprintf fmt ", %a%a"
                  pp_expr h pp_expr_list_inner t
and pp_expr_list : 'a . Format.formatter -> 'a expr_list -> unit =
  fun fmt (type a) (ls: a expr_list) -> match ls with
    | [] -> ()
    | h :: t -> Format.fprintf fmt "%a%a"
                  pp_expr h pp_expr_list_inner t

and pp_ordering_expr_list_inner : 'a . [> `ASC | `DESC] -> Format.formatter -> 'a expr_list -> unit =
  fun ordering fmt (type a) (ls: a expr_list) -> match ls with
    | [] -> ()
    | h :: t -> Format.fprintf fmt ", %a %a%a"
                  pp_expr h pp_ordering ordering
                  (pp_ordering_expr_list_inner ordering) t

and pp_ordering_expr_list : 'a . [> `ASC | `DESC] -> Format.formatter -> 'a expr_list -> unit =
  fun ordering fmt (type a) (ls: a expr_list) -> match ls with
    | [] -> ()
    | h :: t -> Format.fprintf fmt "%a %a%a"
                  pp_expr h pp_ordering ordering
                  (pp_ordering_expr_list_inner ordering) t

and pp_query: 'a 'b. Format.formatter ->
  ('a, 'b) query -> unit =
  fun fmt (type a b) (query: (a,b) query) ->
  (match query with
   | SELECT_CORE { exprs; table; join; where; group_by; having } ->
     Format.fprintf fmt
       "SELECT %a\nFROM %s%a%a%a%a"
       pp_expr_list exprs
       (snd table)
       pp_join_list join
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "WHERE %a" pp_expr vl))
       where
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "GROUP BY %a"
            pp_expr_list vl))
       group_by
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "HAVING %a"
            pp_expr vl))
       having
   | SELECT { core; order_by; limit; offset } ->
     Format.fprintf fmt "%a%a%a%a"
       pp_query core
       (pp_opt (fun fmt (order,vl) ->
          Format.fprintf fmt "ORDER BY %a"
            (pp_ordering_expr_list order) vl))
       order_by
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "LIMIT %a" pp_expr vl))
       limit
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "OFFSET %a" pp_expr vl))
       offset
   | DELETE { table; where } ->
     Format.fprintf fmt "DELETE FROM %s%a"
       (snd table)
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "WHERE %a"
            pp_expr vl
        ))
       where
   | UPDATE { table; on_err; set; where } ->
     Format.fprintf fmt "UPDATE%a %s\nSET %a%a"
       (pp_opt pp_on_err) on_err
       (snd table)
       (Format.pp_print_list ~pp_sep:(fun fmt () ->
          Format.fprintf fmt ", ") pp_wrapped_assign) set
       (pp_opt (fun fmt vl -> Format.fprintf fmt "WHERE %a" pp_expr vl))
       where
   | INSERT { table; on_err; on_conflict; set } ->
     let pp_field : 'a . Format.formatter -> 'a expr -> unit =
       fun fmt (type a) (expr: a expr) : unit ->
         match expr with
         | FIELD (_, field, _) -> Format.fprintf fmt "%s" field
         | _ -> Format.kasprintf failwith "expected field for INSERT query, got %a" pp_expr expr in
     Format.fprintf fmt "INSERT%a INTO %s (%a) VALUES (%a)%a"
       (pp_opt pp_on_err) on_err
       (snd table)
       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
          (fun fmt (ASSIGN (fld, _)) -> Format.fprintf fmt "%a" pp_field (FIELD fld)))
       set
       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
          (fun fmt (ASSIGN (_, expr)) -> Format.fprintf fmt "%a" pp_expr expr))
       set
       (pp_opt pp_on_conflict) on_conflict
  )
and pp_join : int -> Format.formatter -> join -> unit =
  fun n fmt (MkJoin { table; on; join_op }) ->
  Format.fprintf fmt "%a (%a) AS join_tmp_%d ON %a"
    pp_join_op join_op
    pp_query table n
    pp_expr on
and pp_join_list : Format.formatter -> join list -> unit =
  fun fmt ls ->
  match ls with
  | [] -> ()
  | h :: t ->
    Format.fprintf fmt " %a%a" (pp_join 0) h (pp_join_list_inner 1) t
and pp_join_list_inner : int -> Format.formatter -> join list -> unit =
  fun n fmt ls ->
  match ls with
  | [] -> ()
  | h :: t ->
    Format.fprintf fmt " %a%a" (pp_join n) h (pp_join_list_inner (n + 1)) t

and pp_wrapped_assign: Format.formatter -> wrapped_assign -> unit =
  fun fmt (ASSIGN ((_, field_name, _), expr)) ->
  Format.fprintf fmt "%s = %a" field_name pp_expr expr

let rec values_expr_list :
  'a . wrapped_value list -> 'a expr_list -> wrapped_value list =
  fun acc (type a) (exprs: a expr_list) ->
  match exprs with
  | [] -> acc
  | h :: t -> values_expr_list (values_expr acc h) t
and query_values : 'a 'b. wrapped_value list -> ('a,'b) query -> wrapped_value list =
  fun acc (type a b) (query: (a,b) query) ->
  match query with
  | SELECT_CORE { exprs; table=_; join; where; group_by; having } ->
    let acc = values_expr_list acc exprs in
    let acc = List.fold_left (fun acc (MkJoin {table; on; join_op=_}) ->
      let acc = query_values acc table in
      let acc = values_expr acc on in
      acc
    ) acc join in
    let acc = Option.map (values_expr acc) where |> Option.value ~default:acc in
    let acc = Option.map (values_expr_list acc) group_by |> Option.value ~default:acc in
    let acc = Option.map (values_expr acc) having |> Option.value ~default:acc in
    acc
  | SELECT { core; order_by; limit; offset } ->
    let acc = query_values acc core in
    let acc = Option.map (fun (_, expr) -> values_expr_list acc expr) order_by
              |> Option.value ~default:acc in
    let acc = Option.map (values_expr acc) limit |> Option.value ~default:acc in
    let acc = Option.map (values_expr acc) offset |> Option.value ~default:acc in
    acc
  | DELETE { table=_; where } ->
    let acc = Option.map (values_expr acc) where |> Option.value ~default:acc in
    acc
  | UPDATE { table=_; on_err=_; set; where } ->
    let acc = List.fold_left (fun acc (ASSIGN (vl, expr)) ->
      values_expr (values_expr acc (FIELD vl)) expr) acc set in
    let acc = Option.map (values_expr acc) where |> Option.value ~default:acc in
    acc
  | INSERT { table=_; on_err=_; on_conflict=_; set } ->
    let acc = List.fold_left (fun acc (ASSIGN (vl, _)) ->
      values_expr acc (FIELD vl)) acc set in
    let acc = List.fold_left (fun acc (ASSIGN (_, expr)) ->
      values_expr acc expr) acc set in
    acc

module Common = struct

  type 'a expr +=
    | CAST: 'a expr * 'b Type.t -> 'b expr

    | BETWEEN : 'a expr * 'a expr * 'a expr -> bool expr
    | NOT_BETWEEN : 'a expr * 'a expr * 'a expr -> bool expr

    | COUNT: bool * 'b expr_list -> int expr
    | COUNT_STAR: int expr

    | MAX: bool * 'a Type.Numeric.t * 'a expr -> 'a expr
    | MIN: bool * 'a Type.Numeric.t * 'a expr -> 'a expr
    | SUM: bool * 'a Type.Numeric.t * 'a expr -> 'a expr


    | ADD : 'a Type.Numeric.t * 'a expr * 'a expr -> 'a expr
    | SUB : 'a Type.Numeric.t * 'a expr * 'a expr -> 'a expr
    | MUL : 'a Type.Numeric.t * 'a expr * 'a expr -> 'a expr
    | DIV : 'a Type.Numeric.t * 'a expr * 'a expr -> 'a expr
    | ABS : 'a Type.Numeric.t * 'a expr -> 'a expr

    | MOD : 'a Type.Numeric.integral * 'a expr * 'a expr -> 'a expr
    | LAND : 'a Type.Numeric.integral * 'a expr * 'a expr -> 'a expr
    | LOR : 'a Type.Numeric.integral * 'a expr * 'a expr -> 'a expr

    | AND : bool expr * bool expr -> bool expr
    | OR : bool expr * bool expr -> bool expr

    | NOT : bool expr -> bool expr

    | COMPARE : comparison * 'a expr * 'a expr -> bool expr

    | IS_NOT_NULL: 'a expr -> bool expr
    | IS_NULL: 'a expr -> bool expr


    | LOWER: string expr -> string expr
    | UPPER: string expr -> string expr

    | LIKE: string expr * string expr -> bool expr (* args(string, pattern) *)

    | COALESCE: 'a expr list -> 'a expr

    | NULLABLE: 'a expr -> 'a option expr

    | EXISTS : ('a, [> `SELECT | `SELECT_CORE]) query -> bool expr

    | IN : 'a expr * ('a * unit, [> `SELECT | `SELECT_CORE]) query -> bool expr

  let () = add_printer @@ fun pp -> {
    pp_expr=fun (type a) fmt (expr: a expr) : unit ->
      match expr with 
      | CAST (expr, ty) -> Format.fprintf fmt "CAST(%a AS %s)" pp_expr expr (Type.show ty)

      | BETWEEN (expr, lower, upper) -> Format.fprintf fmt "%a BETWEEN %a AND %a" pp_expr expr pp_expr lower pp_expr upper 
      | NOT_BETWEEN (expr, lower, upper) -> Format.fprintf fmt "%a NOT BETWEEN %a AND %a" pp_expr expr pp_expr lower pp_expr upper 

      | COUNT (false, exprs) -> Format.fprintf fmt "COUNT(%a)" pp_expr_list exprs
      | COUNT (true, exprs) -> Format.fprintf fmt "COUNT(DISTINCT %a)" pp_expr_list exprs

      | COUNT_STAR -> Format.fprintf fmt "COUNT( * )"

      | MAX (false, _, expr) -> Format.fprintf fmt "MAX(%a)" pp_expr expr
      | MAX (true, _, expr) -> Format.fprintf fmt "MAX(DISTINCT %a)" pp_expr expr

      | MIN(false, _, expr) -> Format.fprintf fmt "MIN(%a)" pp_expr expr
      | MIN(true, _, expr) -> Format.fprintf fmt "MIN(DISTINCT %a)" pp_expr expr

      | SUM(false, _, expr) -> Format.fprintf fmt "SUM(%a)" pp_expr expr
      | SUM(true, _, expr) -> Format.fprintf fmt "SUM(DISTINCT %a)" pp_expr expr

      | ADD (_, l, r) -> Format.fprintf fmt "(%a) + (%a)" pp_expr l pp_expr r
      | SUB (_, l, r) -> Format.fprintf fmt "(%a) - (%a)" pp_expr l pp_expr r
      | MUL (_, l, r) -> Format.fprintf fmt "(%a) * (%a)" pp_expr l pp_expr r
      | DIV (_, l, r) -> Format.fprintf fmt "(%a) / (%a)" pp_expr l pp_expr r
      | ABS (_, expr) -> Format.fprintf fmt "ABS(%a)" pp_expr expr

      | MOD (_, l, r) -> Format.fprintf fmt "MOD(%a,%a)" pp_expr l pp_expr r
      | LAND (_, l, r) -> Format.fprintf fmt "(%a) & (%a)" pp_expr l pp_expr r
      | LOR (_, l, r) -> Format.fprintf fmt "(%a) | (%a)" pp_expr l pp_expr r

      | AND (l, r) -> Format.fprintf fmt "(%a) AND (%a)" pp_expr l pp_expr r
      | OR (l, r) -> Format.fprintf fmt "(%a) OR (%a)" pp_expr l pp_expr r

      | NOT (e) -> Format.fprintf fmt "NOT (%a)" pp_expr e

      | COMPARE (op, l, r) ->
        let op = match op with
          | EQ -> "="
          | NEQ -> "!="
          | GT -> ">"
          | GE -> ">="
          | LT -> "<"
          | LE -> "<=" in
        Format.fprintf fmt "%a %s %a"
          pp_expr l op pp_expr r

      | IS_NOT_NULL e -> Format.fprintf fmt "%a IS NOT NULL" pp_expr e
      | IS_NULL e -> Format.fprintf fmt "%a IS NULL" pp_expr e


      | LOWER s -> Format.fprintf fmt "LOWER(%a)" pp_expr s
      | UPPER s -> Format.fprintf fmt "UPPER(%a)" pp_expr s

      | LIKE (pat, s) ->
        Format.fprintf fmt "%a LIKE %a" pp_expr s pp_expr pat

      | COALESCE exprs ->
        Format.fprintf fmt "COALESCE(%a)"
          (Format.pp_print_list ~pp_sep:(fun fmt () ->
             Format.fprintf fmt ", ") pp_expr) exprs

      | NULLABLE expr -> pp_expr fmt expr

      | EXISTS query ->
        Format.fprintf fmt "EXISTS (%a)" pp_query query

      | IN (expr, query) ->
        Format.fprintf fmt "(%a) IN (%a)" pp_expr expr pp_query query
      | _ -> pp.pp_expr fmt expr
  }

  let () = add_collector @@ fun collector -> {
    values_expr=fun (type a) acc (e: a expr) : wrapped_value list ->
      match e with
      | CAST (expr, _) -> values_expr acc expr

      | BETWEEN  (expr, lower, upper) ->
        values_expr (values_expr (values_expr acc expr) lower) upper
      | NOT_BETWEEN (expr,lower,upper) -> 
        values_expr (values_expr (values_expr acc expr) lower) upper

      | COUNT (_, exprs) -> values_expr_list acc exprs
      | COUNT_STAR -> acc

      | MAX (_, _, expr) -> values_expr acc expr
      | MIN (_, _, expr) -> values_expr acc expr
      | SUM (_, _, expr) -> values_expr acc expr

      | SUB (_, l, r)
      | ADD (_, l, r)
      | MUL (_, l, r)
      | DIV (_, l, r) ->
        values_expr (values_expr acc l) r

      | ABS (_, expr) -> values_expr acc expr

      | MOD  (_, l, r) -> values_expr (values_expr acc l) r
      | LAND  (_, l, r) -> values_expr (values_expr acc l) r
      | LOR  (_, l, r) -> values_expr (values_expr acc l) r

      | OR (l, r)
      | AND (l, r) ->
        values_expr (values_expr acc l) r

      | NOT expr -> values_expr acc expr

      | COMPARE (_, l, r) ->
        values_expr (values_expr acc l) r

      | IS_NULL expr -> values_expr acc expr
      | IS_NOT_NULL expr -> values_expr acc expr

      | LOWER expr
      | UPPER expr -> values_expr acc expr

      | LIKE (l, r) -> values_expr (values_expr acc l) r

      | COALESCE exprs ->
        List.fold_left values_expr acc exprs

      | NULLABLE expr -> values_expr acc expr

      | EXISTS query ->
        query_values acc query

      | IN  (expr, query) ->
        query_values (values_expr acc expr) query

      | _ -> collector.values_expr acc e
  }


  let () = add_typer @@ fun typer -> {
    ty_expr=fun (type a) (e: a expr) : a Type.t ->
      match e with
      | CAST (_, ty) -> ty
      | BETWEEN _ -> Type.bool
      | NOT_BETWEEN _ -> Type.bool
      | COUNT _ -> Type.int 
      | COUNT_STAR -> Type.int
      | MAX (_, _, expr) -> ty_expr expr
      | MIN (_, _, expr) -> ty_expr expr
      | SUM (_, _, expr) -> ty_expr expr

      | ADD (_, l, _) -> ty_expr l
      | SUB (_, l, _) -> ty_expr l
      | MUL (_, l, _) -> ty_expr l
      | DIV (_, l, _) -> ty_expr l
      | ABS (_, l) -> ty_expr l

      | MOD (_, l, _) -> ty_expr l
      | LAND (_, l, _) -> ty_expr l
      | LOR (_, l, _) -> ty_expr l

      | AND _ -> Type.bool
      | OR _ -> Type.bool
      | NOT _ -> Type.bool

      | COMPARE _ -> Type.bool

      | IS_NOT_NULL _ -> Type.bool
      | IS_NULL _ -> Type.bool
  
      | LOWER expr -> ty_expr expr
      | UPPER expr -> ty_expr expr

      | LIKE _ -> Type.bool

      | COALESCE (h :: _) -> ty_expr h
      | COALESCE [] -> failwith "could not infer type of empty coalesce"

      | NULLABLE expr -> Type.NULLABLE (ty_expr expr)

      | EXISTS _ -> Type.bool

      | IN _ -> Type.bool

      | _ -> typer.ty_expr e
  }

end

module Postgres = struct

  type 'a expr +=

    | BETWEEN_SYMMETRIC : 'a expr * 'a expr * 'a expr -> bool expr
    | NOT_BETWEEN_SYMMETRIC : 'a expr * 'a expr * 'a expr -> bool expr

    | IS_DISTINCT_FROM : 'a expr * 'a expr -> bool expr
    | IS_NOT_DISTINCT_FROM : 'a expr * 'a expr -> bool expr

    | IS_TRUE : bool expr -> bool expr
    | IS_NOT_TRUE : bool expr -> bool expr

    | IS_FALSE : bool expr -> bool expr
    | IS_NOT_FALSE : bool expr -> bool expr

    | IS_UNKNOWN : bool expr -> bool expr
    | IS_NOT_UNKNOWN : bool expr -> bool expr

    | CEIL: 'a Type.Numeric.t * 'a expr -> 'a expr
    | FLOOR: 'a Type.Numeric.t * 'a expr -> 'a expr
    | ROUND: 'a Type.Numeric.t * 'a expr -> 'a expr
    | TRUNC: 'a Type.Numeric.t * 'a expr -> 'a expr
    | PI: float expr
    | SQRT: float expr -> float expr
    | DEGREES: float expr -> float expr
    | RADIANS: float expr -> float expr
    | EXP: float expr -> float expr
    | LN: float expr -> float expr
    | LOG10: float expr -> float expr
    | LOG: float expr * float expr -> float expr (* first argument is base *)
    | POWER : 'a Type.Numeric.t * 'a expr * 'a expr -> 'a expr


    | COS: float expr -> float expr
    | COSD: float expr -> float expr
    | ACOS: float expr -> float expr
    | ACOSD: float expr -> float expr
    | COSH: float expr -> float expr
    | ACOSH: float expr -> float expr

    | SIN: float expr -> float expr
    | SIND: float expr -> float expr
    | ASIN: float expr -> float expr
    | ASIND: float expr -> float expr
    | SINH: float expr -> float expr
    | ASINH: float expr -> float expr


    | TAN: float expr -> float expr
    | TAND: float expr -> float expr
    | ATAN: float expr -> float expr
    | ATAND: float expr -> float expr
    | ATAN2: float expr -> float expr
    | ATAN2D: float expr -> float expr
    | TANH: float expr -> float expr
    | ATANH: float expr -> float expr

    | COT: float expr -> float expr
    | COTD: float expr -> float expr

    | FACTORIAL: 'a Type.Numeric.integral * 'a expr -> 'a expr
    | GCD: 'a Type.Numeric.integral * 'a expr * 'a expr -> 'a expr
    | LCM: 'a Type.Numeric.integral * 'a expr * 'a expr -> 'a expr

    | CONCAT: string expr list -> string expr
    | CONCAT_WS: string expr * string expr list -> string expr
    | CHAR_LENGTH: string expr -> int expr
    | LENGTH: string expr -> int expr

    | SUBSTRING: string expr * int expr option * int expr option -> string expr (* first int is from, second int is for *)
    | REPLACE: string expr * string expr * string expr -> string expr (* args (string,from,to) *)
    | REVERSE: string expr -> string expr
    | STARTS_WITH: string expr * string expr -> bool expr

    | SIMILAR_TO: string expr * string expr -> bool expr (* args(string, pattern) *)

    | GREATEST: 'a Type.Numeric.t * 'a expr list -> 'a expr
    | LEAST: 'a Type.Numeric.t * 'a expr list -> 'a expr

    | RANDOM: float expr

  let () = add_printer @@ fun pp -> {
    pp_expr=fun (type a) fmt (e: a expr) : unit ->
      match e with 

      | BETWEEN_SYMMETRIC (expr, lower, upper) ->
        Format.fprintf fmt "%a BETWEEN SYMMETRIC %a AND %a" pp_expr expr pp_expr lower pp_expr upper
      | NOT_BETWEEN_SYMMETRIC (expr, lower, upper) ->
        Format.fprintf fmt "%a NOT BETWEEN SYMMETRIC %a AND %a" pp_expr expr pp_expr lower pp_expr upper

      | IS_DISTINCT_FROM (l,r) ->
        Format.fprintf fmt "%a IS DISTINCT FROM %a" pp_expr l pp_expr r
      | IS_NOT_DISTINCT_FROM (l,r) -> 
        Format.fprintf fmt "%a IS NOT DISTINCT FROM %a" pp_expr l pp_expr r

      | IS_TRUE expr ->
        Format.fprintf fmt "%a IS TRUE" pp_expr expr
      | IS_NOT_TRUE expr -> 
        Format.fprintf fmt "%a IS NOT TRUE" pp_expr expr

      | IS_FALSE expr ->
        Format.fprintf fmt "%a IS FALSE" pp_expr expr
      | IS_NOT_FALSE expr ->
        Format.fprintf fmt "%a IS NOT FALSE" pp_expr expr

      | IS_UNKNOWN expr -> Format.fprintf fmt "%a IS UNKNOWN" pp_expr expr
      | IS_NOT_UNKNOWN expr -> Format.fprintf fmt "%a IS NOT UNKNOWN" pp_expr expr

      | CEIL (_,expr) -> Format.fprintf fmt "CEIL(%a)" pp_expr expr
      | FLOOR (_,expr) -> Format.fprintf fmt "FLOOR(%a)" pp_expr expr
      | ROUND (_,expr) -> Format.fprintf fmt "ROUND(%a)" pp_expr expr
      | TRUNC (_,expr) -> Format.fprintf fmt "TRUNC(%a)" pp_expr expr
      | PI -> Format.fprintf fmt "PI()"
      | SQRT expr -> Format.fprintf fmt "SQRT(%a)" pp_expr expr
      | DEGREES expr -> Format.fprintf fmt "DEGREES(%a)" pp_expr expr
      | RADIANS expr -> Format.fprintf fmt "RADIANS(%a)" pp_expr expr
      | EXP expr -> Format.fprintf fmt "EXP(%a)" pp_expr expr
      | LN expr -> Format.fprintf fmt "LN(%a)" pp_expr expr
      | LOG10 expr -> Format.fprintf fmt "LOG10(%a)" pp_expr expr
      | LOG (base,x) -> Format.fprintf fmt "LOG(%a,%a)" pp_expr base pp_expr x
      | POWER (_, a,b) -> Format.fprintf fmt "POWER(%a,%a)" pp_expr a pp_expr b (* a ^ b *)


      | COS expr -> Format.fprintf fmt "COS(%a)" pp_expr expr
      | COSD expr -> Format.fprintf fmt "COSD(%a)" pp_expr expr
      | ACOS expr -> Format.fprintf fmt "ACOS(%a)" pp_expr expr
      | ACOSD expr -> Format.fprintf fmt "ACOSD(%a)" pp_expr expr
      | COSH expr -> Format.fprintf fmt "COSH(%a)" pp_expr expr
      | ACOSH expr -> Format.fprintf fmt "ACOSH(%a)" pp_expr expr

      | SIN expr -> Format.fprintf fmt "SIN(%a)" pp_expr expr
      | SIND expr -> Format.fprintf fmt "SIND(%a)" pp_expr expr
      | ASIN expr -> Format.fprintf fmt "ASIN(%a)" pp_expr expr
      | ASIND expr -> Format.fprintf fmt "ASIND(%a)" pp_expr expr
      | SINH expr -> Format.fprintf fmt "SINH(%a)" pp_expr expr
      | ASINH expr -> Format.fprintf fmt "ASINH(%a)" pp_expr expr


      | TAN expr -> Format.fprintf fmt "TAN(%a)" pp_expr expr
      | TAND expr -> Format.fprintf fmt "TAND(%a)" pp_expr expr
      | ATAN expr -> Format.fprintf fmt "ATAN(%a)" pp_expr expr
      | ATAND expr -> Format.fprintf fmt "ATAND(%a)" pp_expr expr
      | ATAN2 expr -> Format.fprintf fmt "ATAN2(%a)" pp_expr expr
      | ATAN2D expr -> Format.fprintf fmt "ATAN2D(%a)" pp_expr expr
      | TANH expr -> Format.fprintf fmt "TANH(%a)" pp_expr expr
      | ATANH expr -> Format.fprintf fmt "ATANH(%a)" pp_expr expr

      | COT expr -> Format.fprintf fmt "COT(%a)" pp_expr expr
      | COTD expr -> Format.fprintf fmt "COTD(%a)" pp_expr expr

      | FACTORIAL (_, expr) -> Format.fprintf fmt "FACTORIAL(%a)" pp_expr expr
      | GCD (_, l,r) -> Format.fprintf fmt "GCD(%a,%a)" pp_expr l pp_expr r
      | LCM (_, l,r) -> Format.fprintf fmt "LCM(%a,%a)" pp_expr l pp_expr r

      | CONCAT exprs ->
        Format.fprintf fmt "CONCAT(%a)"
          (Format.pp_print_list ~pp_sep:(fun fmt () ->
             Format.fprintf fmt ", ") pp_expr) exprs
      | CONCAT_WS (sep, []) -> Format.fprintf fmt "CONCAT_WS(%a)" pp_expr sep
      | CONCAT_WS (sep, exprs) ->
        Format.fprintf fmt "CONCAT_WS(%a,%a)" pp_expr sep 
          (Format.pp_print_list ~pp_sep:(fun fmt () ->
             Format.fprintf fmt ", ") pp_expr) exprs

      | CHAR_LENGTH expr ->
        Format.fprintf fmt "CHAR_LENGTH(%a)" pp_expr expr
      | LENGTH expr -> Format.fprintf fmt "LENGTH(%a)" pp_expr expr

      | SUBSTRING (text, None, None) -> Format.fprintf fmt "SUBSTRING(%a)" pp_expr text
      | SUBSTRING (text, Some from, None) -> Format.fprintf fmt "SUBSTRING(%a FROM %a)" pp_expr text pp_expr from
      | SUBSTRING (text, None, Some for_) -> Format.fprintf fmt "SUBSTRING(%a FOR %a)" pp_expr text pp_expr for_
      | SUBSTRING (text, Some from, Some for_) -> Format.fprintf fmt "SUBSTRING(%a FROM %a FOR %a)" pp_expr text pp_expr from pp_expr for_

      | REPLACE (text,from,to_) -> Format.fprintf fmt "REPLACE(%a,%a,%a)" pp_expr text pp_expr from pp_expr to_
      | REVERSE expr -> Format.fprintf fmt "REVERSE(%a)" pp_expr expr
      | STARTS_WITH (text,prefix) -> Format.fprintf fmt "STARTS_WITH(%a,%a)" pp_expr text pp_expr prefix

      | SIMILAR_TO (text,pat) -> Format.fprintf fmt "(%a) SIMILAR TO (%a)" pp_expr text pp_expr pat

      | GREATEST (_, exprs) ->
        Format.fprintf fmt "GREATEST(%a)"
          (Format.pp_print_list ~pp_sep:(fun fmt () ->
             Format.fprintf fmt ", ") pp_expr) exprs
      | LEAST (_,exprs) ->
        Format.fprintf fmt "LEAST(%a)"
          (Format.pp_print_list ~pp_sep:(fun fmt () ->
             Format.fprintf fmt ", ") pp_expr) exprs

      | RANDOM -> Format.fprintf fmt "RANDOM()"

      | _ -> pp.pp_expr fmt e
  }

  let () = add_collector @@ fun collector -> {
    values_expr=fun (type a) acc (e: a expr) : wrapped_value list ->
      match e with 
      | BETWEEN_SYMMETRIC (expr, lower, upper) ->
        values_expr (values_expr (values_expr acc expr) lower) upper
      | NOT_BETWEEN_SYMMETRIC (expr, lower, upper) ->
        values_expr (values_expr (values_expr acc expr) lower) upper

      | IS_DISTINCT_FROM (l,r) ->
        values_expr (values_expr acc l) r
      | IS_NOT_DISTINCT_FROM (l,r) -> 
        values_expr (values_expr acc l) r

      | IS_TRUE expr -> values_expr acc expr
      | IS_NOT_TRUE expr -> values_expr acc expr

      | IS_FALSE expr -> values_expr acc expr
      | IS_NOT_FALSE expr -> values_expr acc expr

      | IS_UNKNOWN expr -> values_expr acc expr
      | IS_NOT_UNKNOWN expr -> values_expr acc expr

      | CEIL (_,expr) -> values_expr acc expr
      | FLOOR (_,expr) -> values_expr acc expr
      | ROUND (_,expr) -> values_expr acc expr
      | TRUNC (_,expr) -> values_expr acc expr
      | PI -> acc
      | SQRT expr -> values_expr acc expr
      | DEGREES expr -> values_expr acc expr
      | RADIANS expr -> values_expr acc expr
      | EXP expr -> values_expr acc expr
      | LN expr -> values_expr acc expr
      | LOG10 expr -> values_expr acc expr
      | LOG (base,x) -> values_expr (values_expr acc base) x
      | POWER (_, a,b) -> values_expr (values_expr acc a) b (* a ^ b *)


      | COS expr -> values_expr acc expr
      | COSD expr -> values_expr acc expr
      | ACOS expr -> values_expr acc expr
      | ACOSD expr -> values_expr acc expr
      | COSH expr -> values_expr acc expr
      | ACOSH expr -> values_expr acc expr

      | SIN expr -> values_expr acc expr
      | SIND expr -> values_expr acc expr
      | ASIN expr -> values_expr acc expr
      | ASIND expr -> values_expr acc expr
      | SINH expr -> values_expr acc expr
      | ASINH expr -> values_expr acc expr


      | TAN expr -> values_expr acc expr
      | TAND expr -> values_expr acc expr
      | ATAN expr -> values_expr acc expr
      | ATAND expr -> values_expr acc expr
      | ATAN2 expr -> values_expr acc expr
      | ATAN2D expr -> values_expr acc expr
      | TANH expr -> values_expr acc expr
      | ATANH expr -> values_expr acc expr

      | COT expr -> values_expr acc expr
      | COTD expr -> values_expr acc expr

      | FACTORIAL (_, expr) -> values_expr acc expr
      | GCD (_, l,r) -> values_expr (values_expr acc l) r
      | LCM (_, l,r) -> values_expr (values_expr acc l) r

      | CONCAT exprs ->
        List.fold_left (fun acc expr -> values_expr acc expr) acc exprs
      | CONCAT_WS (sep, []) -> values_expr acc sep
      | CONCAT_WS (sep, exprs) ->
        List.fold_left (fun acc expr -> values_expr acc expr)
          (values_expr acc sep) exprs
      | CHAR_LENGTH expr -> values_expr acc expr

      | LENGTH expr -> values_expr acc expr

      | SUBSTRING (text, None, None) -> values_expr acc text
      | SUBSTRING (text, Some from, None) -> values_expr (values_expr acc text) from
      | SUBSTRING (text, None, Some for_) ->  values_expr (values_expr acc text) for_
      | SUBSTRING (text, Some from, Some for_) -> values_expr (values_expr (values_expr acc text) from) for_

      | REPLACE (text,from,to_) ->
        values_expr (values_expr (values_expr acc text) from) to_
      | REVERSE expr -> values_expr acc expr
      | STARTS_WITH (text,prefix) ->
        values_expr (values_expr acc text) prefix

      | SIMILAR_TO (text,pat) ->
        values_expr (values_expr acc text) pat


      | GREATEST (_, exprs) ->
        List.fold_left (fun acc expr -> values_expr acc expr) acc exprs
      | LEAST (_,exprs) ->
        List.fold_left (fun acc expr -> values_expr acc expr) acc exprs

      | RANDOM -> acc

      | _ -> collector.values_expr acc e
  }

  let () = add_typer @@ fun typer -> {
    ty_expr=fun (type a) (e: a expr) : a Type.t ->
      match e with
      | BETWEEN_SYMMETRIC (_, _, _) -> Type.bool
      | NOT_BETWEEN_SYMMETRIC (_, _, _) -> Type.bool

      | IS_DISTINCT_FROM (_,_) -> Type.bool
      | IS_NOT_DISTINCT_FROM (_,_) -> Type.bool

      | IS_TRUE _ -> Type.bool
      | IS_NOT_TRUE _ -> Type.bool

      | IS_FALSE _ -> Type.bool
      | IS_NOT_FALSE _ -> Type.bool

      | IS_UNKNOWN _ -> Type.bool
      | IS_NOT_UNKNOWN _ -> Type.bool

      | CEIL (_,expr) -> ty_expr expr
      | FLOOR (_,expr) -> ty_expr expr
      | ROUND (_,expr) -> ty_expr expr
      | TRUNC (_,expr) -> ty_expr expr
      | PI -> Type.REAL
      | SQRT expr -> ty_expr expr
      | DEGREES expr -> ty_expr expr
      | RADIANS expr -> ty_expr expr
      | EXP expr -> ty_expr expr
      | LN expr -> ty_expr expr
      | LOG10 expr -> ty_expr expr
      | LOG (base,_) -> ty_expr base
      | POWER (_, a,_) -> ty_expr a


      | COS _ -> Type.REAL
      | COSD _ -> Type.REAL
      | ACOS _ -> Type.REAL
      | ACOSD _ -> Type.REAL
      | COSH _ -> Type.REAL
      | ACOSH _ -> Type.REAL

      | SIN _ -> Type.REAL
      | SIND _ -> Type.REAL
      | ASIN _ -> Type.REAL
      | ASIND _ -> Type.REAL
      | SINH _ -> Type.REAL
      | ASINH _ -> Type.REAL


      | TAN _ -> Type.REAL
      | TAND _ -> Type.REAL
      | ATAN _ -> Type.REAL
      | ATAND _ -> Type.REAL
      | ATAN2 _ -> Type.REAL
      | ATAN2D _ -> Type.REAL
      | TANH _ -> Type.REAL
      | ATANH _ -> Type.REAL

      | COT _ -> Type.REAL
      | COTD _ -> Type.REAL

      | FACTORIAL (_, expr) -> ty_expr expr
      | GCD (_, l,_) -> ty_expr l
      | LCM (_, l,_) -> ty_expr l

      | CONCAT _ -> Type.TEXT

      | CONCAT_WS (_, _) -> Type.TEXT

      | CHAR_LENGTH _ -> Type.INTEGER

      | LENGTH _ -> Type.INTEGER

      | SUBSTRING (_, _, _) -> Type.TEXT

      | REPLACE (_,_,_) -> Type.TEXT

      | REVERSE _ -> Type.TEXT
      | STARTS_WITH (_,_) -> Type.bool
      | SIMILAR_TO (_,_) -> Type.bool


      | GREATEST (_, expr :: _) -> ty_expr expr
      | LEAST (_,expr :: _) -> ty_expr expr

      | GREATEST (_, []) -> failwith "invalid syntax -- empty GREATEST expression"
      | LEAST (_,[]) -> failwith "invalid syntax -- empty LEAST expression"

      | RANDOM -> Type.REAL

      | _ -> typer.ty_expr e
  }

end

module Sqlite3 = struct

  type 'a expr +=
    | TOTAL: bool * int expr -> int expr
    | GROUP_CONCAT: bool * string expr * string expr option -> string expr


    | CHANGES: int expr
    | GLOB: string expr * string expr -> bool expr
    | MIN_OF: 'a Type.Numeric.t * 'a expr list -> 'a expr
    | MAX_OF: 'a Type.Numeric.t * 'a expr list -> 'a expr

    | RANDOM: int expr


  let () = add_printer @@ fun pp -> {
    pp_expr=fun (type a) fmt (expr: a expr) : unit ->
      match expr with 
      | TOTAL (false, expr) ->
        Format.fprintf fmt "TOTAL(%a)" pp_expr expr
      | TOTAL (true, expr) ->
        Format.fprintf fmt "TOTAL(DISTINCT %a)" pp_expr expr

      | GROUP_CONCAT (false, l, None) ->
        Format.fprintf fmt "GROUP_CONCAT(%a)" pp_expr l
      | GROUP_CONCAT (false, l, Some r) ->
        Format.fprintf fmt "GROUP_CONCAT(%a, %a)" pp_expr l pp_expr r

      | GROUP_CONCAT (true, l, None) ->
        Format.fprintf fmt "GROUP_CONCAT(DISTINCT %a)" pp_expr l
      | GROUP_CONCAT (true, l, Some r) ->
        Format.fprintf fmt "GROUP_CONCAT(DISTINCT %a, %a)" pp_expr l pp_expr r

      | CHANGES -> Format.fprintf fmt "CHANGES()"

      | GLOB (pat, expr) ->
        Format.fprintf fmt "GLOB(%a, %a)" pp_expr pat pp_expr expr

      | MIN_OF (_, exprs) ->
        Format.fprintf fmt "MIN(%a)"
          (Format.pp_print_list ~pp_sep:(fun fmt () ->
             Format.fprintf fmt ", ") pp_expr) exprs

      | MAX_OF (_, exprs) ->
        Format.fprintf fmt "MAX(%a)"
          (Format.pp_print_list ~pp_sep:(fun fmt () ->
             Format.fprintf fmt ", ") pp_expr) exprs

      | RANDOM ->
        Format.fprintf fmt "RANDOM()"
      | _ -> pp.pp_expr fmt expr
  }

  let () = add_collector @@ fun collector -> {
    values_expr=fun (type a) acc (e: a expr)  : wrapped_value list ->
      match e with

      | TOTAL (_, expr) -> values_expr acc expr

      | GROUP_CONCAT (_, l, None) ->
        values_expr acc l
      | GROUP_CONCAT (_, l, Some r) ->
        values_expr (values_expr acc l) r

      | CHANGES -> acc
      | GLOB (l, r) -> values_expr (values_expr acc l) r

      | MIN_OF (_, exprs) ->
        List.fold_left values_expr acc exprs
      | MAX_OF (_, exprs) ->
        List.fold_left values_expr acc exprs

      | RANDOM -> acc

      | _ -> collector.values_expr acc e
  }

  let () = add_typer @@ fun typer -> {
    ty_expr=fun (type a) (e: a expr) : a Type.t ->
      match e with
      | TOTAL (_, expr) -> ty_expr expr

      | GROUP_CONCAT (_, _, _) -> Type.TEXT

      | CHANGES -> Type.INTEGER
      | GLOB (_, _) -> Type.bool

      | MIN_OF (_, expr :: _) -> ty_expr expr
      | MAX_OF (_, expr :: _) -> ty_expr expr

      | MIN_OF (_, []) -> failwith "invalid syntax -- empty MIN(..)"
      | MAX_OF (_, []) -> failwith "invalid syntax -- empty MAX(..)"


      | RANDOM -> Type.INTEGER
      | _ -> typer.ty_expr e
  }

end
