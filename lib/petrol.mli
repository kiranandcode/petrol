type table_name

module Type : sig

  type 'a t = 'a Type.t

  val bool : bool t

  val int : int t

  val real : float t

  val text : string t

  val blob : string t

end 


module Expr : sig

  type 'a t = 'a Expr.t

  type 'a expr_list =
    | [] : unit expr_list
    | (::) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

  val pp: Format.formatter -> 'a t -> unit

  val i : int -> int t
  val f : float -> float t
  val s : string -> string t
  val b : string -> string t
  val bl : bool -> bool t

  val i_stat : int -> int t

  val f_stat : float -> float t

  val s_stat : string -> string t

  val b_stat : string -> string t

  val nullable: 'a t -> 'a option t

  val true_ : bool t

  val false_ : bool t

  val ( + ) : int t -> int t -> int t
  val ( - ) : int t -> int t -> int t

  val ( = ) : 'a t -> 'a t -> bool t
  val ( <> ) : 'a t -> 'a t -> bool t
  val ( <= ) : 'a t -> 'a t -> bool t
  val ( < ) : 'a t -> 'a t -> bool t
  val ( > ) : 'a t -> 'a t -> bool t
  val ( >= ) : 'a t -> 'a t -> bool t
  val ( && ) : bool t -> bool t -> bool t
  val ( || ) : bool t -> bool t -> bool t


  val is_not_null : 'a t -> bool t

  val coerce : 'a t -> 'b Type.t -> 'b t

  val as_ : 'a t -> name:string -> 'a t * 'a t

  val count : ?distinct:bool -> 'a expr_list -> int t

  val count_star : int t

  val max : ?distinct:bool -> int t -> int t

  val min : ?distinct:bool -> int t -> int t

  val sum : ?distinct:bool -> int t -> int t

  val total : ?distinct:bool -> int t -> int t

  val group_concat: ?distinct:bool -> ?sep_by:string t -> string t -> string t

  val abs : int t -> int t

  val changes : int t

  val glob : pat:string t -> string t -> bool t

  val coalesce : 'a t list -> 'a t

  val like : string t -> pat:string t -> bool t

  val max_of : int t list -> int t

  val min_of : int t list -> int t

  val random : int t

  val lower : string t -> string t

  val upper : string t -> string t

end


module Schema : sig

  type conflict_clause =
    [ `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ]
  type foreign_conflict_clause =
    [ `CASCADE | `NO_ACTION | `RESTRICT | `SET_DEFAULT | `SET_NULL ]

  type 'a constraint_

  type 'a field

  val field : ?constraints:[`Column] constraint_ list -> string -> ty:'a Type.t -> 'a field

  type 'a table =
    [] : unit table
  | (::) : ('a field * 'b table) -> ('a * 'b) table

  val primary_key :
    ?name:string ->
    ?ordering:[ `ASC | `DESC ] ->
    ?on_conflict:conflict_clause ->
    ?auto_increment:bool -> unit -> [ `Column ] constraint_

  val table_primary_key :
    ?name:string ->
    ?on_conflict:conflict_clause -> string list -> [ `Table ] constraint_

  val not_null :
    ?name:string ->
    ?on_conflict:conflict_clause -> unit -> [ `Column ] constraint_

  val unique :
    ?name:string ->
    ?on_conflict:conflict_clause -> unit -> [ `Column ] constraint_

  val table_unique :
    ?name:string ->
    ?on_conflict:conflict_clause -> string list -> [ `Table ] constraint_

  val foreign_key :
    ?name:string ->
    ?on_update:foreign_conflict_clause ->
    ?on_delete:foreign_conflict_clause ->
    table:table_name ->
    columns:'a Expr.expr_list -> unit -> [ `Column ] constraint_

  val table_foreign_key :
    ?name:string ->
    ?on_update:foreign_conflict_clause ->
    ?on_delete:foreign_conflict_clause ->
    table:table_name ->
    columns:'a Expr.expr_list -> string list -> [ `Table ] constraint_

end

module Query : sig

  type ('ret_ty, 'query_tag) t
  type join_op = LEFT | RIGHT | INNER
  type wrapped_assign

  type ('a, 'c) filter_fun = bool Expr.t -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [< `DELETE | `SELECT | `SELECT_CORE | `UPDATE ]

  type ('a, 'b, 'c) group_by_fun =
    'b Expr.expr_list -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [< `SELECT | `SELECT_CORE ]

  type ('a, 'c) having_fun = bool Expr.t -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [< `SELECT | `SELECT_CORE ]

  type ('a, 'b, 'd, 'c) join_fun =
    ?op:join_op ->
    on:bool Expr.t -> ('b, 'd) t -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [< `SELECT_CORE ] constraint 'd = [< `SELECT_CORE | `SELECT ]

  type ('a, 'b, 'c) on_err_fun = 'b -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [> `INSERT | `UPDATE ]
    constraint 'b = [< `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ]

  val (:=) : 'a Expr.t -> 'a Expr.t -> wrapped_assign

  val select :
    'a Expr.expr_list -> from:table_name -> ('a, [> `SELECT_CORE ]) t

  val update :
    table:table_name -> set:wrapped_assign list -> (unit, [> `UPDATE ]) t

  val insert :
    table:table_name ->
    values:wrapped_assign list -> (unit, [> `INSERT ]) t

  val delete : from:table_name -> (unit, [> `DELETE ]) t

  val filter :
    ([< `DELETE | `SELECT | `SELECT_CORE | `UPDATE ], 'c) filter_fun

  val group_by : ([< `SELECT | `SELECT_CORE ], 'b, 'c) group_by_fun

  val having : ([< `SELECT | `SELECT_CORE ], 'c) having_fun

  val join : ([ `SELECT_CORE ], 'b, [< `SELECT_CORE | `SELECT ], 'c) join_fun

  val on_err : [ `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ] ->
    (unit, 'a) t -> (unit, 'a) t

  val limit :
    int Expr.t -> ('a, [ `SELECT | `SELECT_CORE ]) t -> ('a, [> `SELECT ]) t

  val offset
    : int Expr.t -> ('a, [ `SELECT | `SELECT_CORE ]) t -> ('a, [> `SELECT ]) t

  val order_by
    : ?direction:[ `ASC | `DESC ] -> 'a Expr.t -> ('b, [ `SELECT | `SELECT_CORE ]) t -> ('b, [> `SELECT ]) t

end

module Request : sig

  type ('res, 'multiplicity) t

  val make_zero : (unit, 'b) Query.t -> (unit, [ `Zero ]) t

  val make_one : ('a, 'b) Query.t -> ('a, [ `One ]) t

  val make_zero_or_one : ('a, 'b) Query.t -> ('a, [ `One | `Zero ]) t

  val make_many : ('a, 'b) Query.t -> ('a, [ `Many | `One | `Zero ]) t

end

val exec : (module Caqti_lwt.CONNECTION) -> (unit, [< `Zero ]) Request.t ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val find : (module Caqti_lwt.CONNECTION) -> ('a, [< `One ]) Request.t ->
  ('a, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val find_opt : (module Caqti_lwt.CONNECTION) ->
  ('a, [< `One | `Zero ]) Request.t ->
  ('a option, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val collect_list :
  (module Caqti_lwt.CONNECTION) ->
  ('a, [< `Many | `One | `Zero ]) Request.t ->
  ('a list, [> Caqti_error.call_or_retrieve ]) result Lwt.t


module Database : sig

  val declare_table :
    ?constraints:[`Table] Schema.constraint_ list -> name:string -> 'a Schema.table -> Types.table_name * 'a Expr.expr_list

  val initialise : (module Caqti_lwt.CONNECTION) -> (unit, [> `Msg of string ]) Lwt_result.t

  module Versioned : sig

  end

end
