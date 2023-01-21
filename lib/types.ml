type table_name = int * string

type comparison = EQ | NEQ
                | GT | GE
                | LT | LE

type join_op = LEFT | RIGHT | INNER 
let pp_join_op fmt = function
  | LEFT -> Format.fprintf fmt "LEFT JOIN"
  | RIGHT -> Format.fprintf fmt "RIGHT JOIN"
  | INNER -> Format.fprintf fmt "INNER JOIN"

type 'a field = table_name * string * 'a Type.t

type 'a expr_list =
  | [] : unit expr_list
  | (::) : ('a expr * 'b expr_list) -> ('a * 'b) expr_list

and 'a expr =
  | NULL : 'a option Type.t -> 'a option expr
  | ADD : int expr * int expr -> int expr
  | SUB : int expr * int expr -> int expr

  | AND : bool expr * bool expr -> bool expr
  | OR : bool expr * bool expr -> bool expr
  | COMPARE : comparison * 'a expr * 'a expr -> bool expr
  | IS_NOT_NULL: 'a expr -> bool expr
  | NOT : bool expr -> bool expr
  | EXISTS : ('a, [> `SELECT | `SELECT_CORE]) query -> bool expr


  | FIELD : 'a field -> 'a expr
  | CONST : 'a * 'a Type.t -> 'a expr
  | CONST_STATIC : 'a * 'a Type.t -> 'a expr
  | COERCETO: 'a expr * 'b Type.t -> 'b expr
  | AS: 'a expr * string -> 'a expr
  | REF : string * 'a Type.t -> 'a expr
  | NULLABLE: 'a expr -> 'a option expr

  | COUNT: bool * 'b expr_list -> int expr
  | COUNT_STAR: int expr
  | MAX: bool * int expr -> int expr
  | MIN: bool * int expr -> int expr
  | SUM: bool * int expr -> int expr
  | TOTAL: bool * int expr -> int expr
  | GROUP_CONCAT: bool * string expr * string expr option -> string expr

  | ABS : int expr -> int expr
  | CHANGES: int expr
  | GLOB: string expr * string expr -> bool expr
  | COALESCE: 'a expr list -> 'a expr
  | LIKE: string expr * string expr -> bool expr
  | MIN_OF: int expr list -> int expr
  | MAX_OF: int expr list -> int expr
  | RANDOM: int expr
  | LOWER: string expr -> string expr
  | UPPER: string expr -> string expr
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
      set: wrapped_assign list;
    } -> (unit, [> `INSERT] as 'res) query

and join = MkJoin: {
    table: ('r, [< `SELECT_CORE | `SELECT ]) query;
    on: bool expr;
    join_op: join_op;
  } -> join

type wrapped_value = MkWrapped: 'a Type.t * 'a -> wrapped_value

let rec values_expr : 'a . wrapped_value list -> 'a expr
  -> wrapped_value list =
  fun acc (type a) (expr: a expr) ->
  match expr with
  | NULL _ -> acc
  | NULLABLE expr -> values_expr acc expr
  | COMPARE (_, l, r) ->
    values_expr (values_expr acc l) r
  | SUB (l, r)
  | ADD (l, r) ->
    values_expr (values_expr acc l) r
  | OR (l, r)
  | AND (l, r) ->
    values_expr (values_expr acc l) r
  | IS_NOT_NULL expr ->
    values_expr acc expr
  | NOT expr -> values_expr acc expr
  | EXISTS query ->
    query_values acc query
  | FIELD _ -> acc
  | CONST (vl, ty) ->
    (MkWrapped (ty,vl)) :: acc
  | CONST_STATIC (_, _) -> acc
  | COERCETO (expr, _) -> values_expr acc expr
  | AS (expr, _) -> values_expr acc expr
  | REF _ -> acc
  | COUNT (_, exprs) -> values_expr_list acc exprs
  | COUNT_STAR -> acc
  | GROUP_CONCAT (_, l, None) ->
    values_expr acc l
  | GROUP_CONCAT (_, l, Some r) ->
    values_expr (values_expr acc l) r
  | MAX (_, expr) -> values_expr acc expr
  | MIN (_, expr) -> values_expr acc expr
  | SUM (_, expr) -> values_expr acc expr
  | TOTAL (_, expr) -> values_expr acc expr
  | ABS expr -> values_expr acc expr
  | CHANGES -> acc
  | GLOB (l, r) -> values_expr (values_expr acc l) r
  | COALESCE exprs ->
    List.fold_left values_expr acc exprs
  | LIKE (l, r) -> values_expr (values_expr acc l) r
  | MIN_OF exprs ->
    List.fold_left values_expr acc exprs
  | MAX_OF exprs ->
    List.fold_left values_expr acc exprs
  | RANDOM -> acc
  | LOWER expr
  | UPPER expr -> values_expr acc expr
and values_expr_list :
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
  | INSERT { table=_; on_err=_; set } ->
    let acc = List.fold_left (fun acc (ASSIGN (vl, _)) ->
        values_expr acc (FIELD vl)) acc set in
    let acc = List.fold_left (fun acc (ASSIGN (_, expr)) ->
        values_expr acc expr) acc set in
    acc

let pp_opt f fmt = function
  | None -> ()
  | Some vl -> Format.fprintf fmt "\n%a" f vl

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

let rec pp_expr  : 'a . Format.formatter -> 'a expr -> unit =
  fun fmt (type a) (expr: a expr) ->
  match expr with
  | NULL _ -> Format.fprintf fmt "NULL"
  | NULLABLE expr -> pp_expr fmt expr
  | NOT expr -> Format.fprintf fmt "NOT %a" pp_expr expr
  | EXISTS query ->
    Format.fprintf fmt "EXISTS (%a)" pp_query query
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
  | ADD (l, r) ->
    Format.fprintf fmt "(%a) + (%a)"
      pp_expr l pp_expr r
  | SUB (l, r) ->
    Format.fprintf fmt "(%a) - (%a)"
      pp_expr l pp_expr r
  | AND (l,r) ->
    Format.fprintf fmt "(%a) AND (%a)"
      pp_expr l pp_expr r
  | OR (l, r) ->
    Format.fprintf fmt "(%a) OR (%a)"
      pp_expr l pp_expr r
  | IS_NOT_NULL expr -> 
    Format.fprintf fmt "%a IS NOT NULL"
      pp_expr expr
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
  | COERCETO (expr, _) -> pp_expr fmt expr
  | AS (expr, name) ->
    Format.fprintf fmt "%a AS %s" pp_expr expr name
  | REF (name,_) ->
    Format.fprintf fmt "%s" name
  | COUNT (false, exprs) ->
    Format.fprintf fmt "COUNT(%a)" pp_expr_list exprs
  | COUNT (true, exprs) -> 
    Format.fprintf fmt "COUNT(DISTINCT %a)" pp_expr_list exprs
  | COUNT_STAR ->
    Format.fprintf fmt "COUNT(*)"
  | MAX (false, expr) -> 
    Format.fprintf fmt "MAX(%a)" pp_expr expr
  | MAX (true, expr) -> 
    Format.fprintf fmt "MAX(DISTINCT %a)" pp_expr expr
  | MIN (false, expr) -> 
    Format.fprintf fmt "MIN(%a)" pp_expr expr
  | MIN (true, expr) -> 
    Format.fprintf fmt "MIN(DISTINCT %a)" pp_expr expr
  | SUM (false, expr) -> 
    Format.fprintf fmt "SUM(%a)" pp_expr expr
  | SUM (true, expr) -> 
    Format.fprintf fmt "SUM(DISTINCT %a)" pp_expr expr
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
  | ABS expr ->
    Format.fprintf fmt "ABS(%a)" pp_expr expr
  | CHANGES -> Format.fprintf fmt "CHANGES()"
  | GLOB (pat, expr) ->
    Format.fprintf fmt "GLOB(%a, %a)" pp_expr pat pp_expr expr
  | COALESCE exprs ->
    Format.fprintf fmt "COALESCE(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () ->
         Format.fprintf fmt ", ") pp_expr) exprs
  | LIKE (pat, s) ->
    Format.fprintf fmt "LIKE(%a,%a)" pp_expr pat pp_expr s
  | MIN_OF exprs ->
    Format.fprintf fmt "MIN(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () ->
         Format.fprintf fmt ", ") pp_expr) exprs
  | MAX_OF exprs -> 
    Format.fprintf fmt "MAX(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () ->
         Format.fprintf fmt ", ") pp_expr) exprs
  | RANDOM ->
    Format.fprintf fmt "RANDOM()"
  | LOWER s ->
    Format.fprintf fmt "LOWER(%a)" pp_expr s
  | UPPER s ->
    Format.fprintf fmt "UPPER(%a)" pp_expr s
and pp_expr_list_inner : 'a . Format.formatter -> 'a expr_list -> unit =
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
   | INSERT { table; on_err; set } ->
     let pp_field : 'a . Format.formatter -> 'a expr -> unit =
       fun fmt (type a) (expr: a expr) : unit ->
         match expr with
         | FIELD (_, field, _) -> Format.fprintf fmt "%s" field
         | _ -> Format.kasprintf failwith "expected field for INSERT query, got %a" pp_expr expr in
     Format.fprintf fmt "INSERT%a INTO %s (%a) VALUES (%a)"
       (pp_opt pp_on_err) on_err
       (snd table)
       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
          (fun fmt (ASSIGN (fld, _)) -> Format.fprintf fmt "%a" pp_field (FIELD fld)))
       set
       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
          (fun fmt (ASSIGN (_, expr)) -> Format.fprintf fmt "%a" pp_expr expr))
       set)
and pp_join : Format.formatter -> join -> unit =
  fun fmt (MkJoin { table; on; join_op }) ->
  Format.fprintf fmt "%a (%a) ON %a"
    pp_join_op join_op
    pp_query table
    pp_expr on
and pp_join_list : Format.formatter -> join list -> unit =
  fun fmt ls ->
  match ls with
  | [] -> ()
  | h :: t ->
    Format.fprintf fmt " %a%a" pp_join h pp_join_list t
and pp_wrapped_assign: Format.formatter -> wrapped_assign -> unit =
  fun fmt (ASSIGN ((_, field_name, _), expr)) ->
  Format.fprintf fmt "%s = %a" field_name pp_expr expr
