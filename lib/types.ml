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
  | ADD : int expr * int expr -> int expr
  | SUB : int expr * int expr -> int expr

  | COMPARE : comparison * 'a expr * 'a expr -> bool expr
  | AND : bool expr * bool expr -> bool expr
  | OR : bool expr * bool expr -> bool expr
  | IS_NOT_NULL: 'a expr -> bool expr
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
and wrapped_assign = ASSIGN : 'a expr * 'a expr -> wrapped_assign
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
      order_by: ([`ASC | `DESC] * 'e expr) option;
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
