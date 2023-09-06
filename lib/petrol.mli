type table_name
(** Uniquely identifies a table in the system.  *)

module Expr : sig

  (** Defines an extensible embedding of SQL expressions.  *)

  type 'a t
  (** ['a t] represents an SQL expression that evaluates to a value of
      OCaml type ['a] *)

  val pp: Format.formatter -> 'a t -> unit
  (** [pp fmt expr] pretty prints the expression [expr].

      {b Note} You shouldn't have to call this yourself. Petrol usually
      takes care of this for you, but you may want to use it for
      debugging. *)

  type wrapped_assign
  (** An opaque wrapper that represents an assignment of a value to a
      particular field in a table.  *)

  (** Represents a heterogeneous sequence of SQL expressions. *)
  type 'a expr_list =
    | [] : unit expr_list
    | (::) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

  val pp_expr_list : Format.formatter -> 'a expr_list -> unit
  (** [pp_expr_list fmt exps] pretty prints the expression list [exps]
      of type {!expr_list}.

      See also {!t} and {!pp}. *)


end

module Type : sig

  type 'a t
  (** Represents an SQL data type that maps to an OCaml type ['a]. *)

  val pp: Format.formatter -> 'a t -> unit
  val show : 'a t -> string

  val custom : ty:'a Caqti_type.t -> repr:string -> 'a t
  (** [custom ~ty ~repr] creates a new SQL type that is represented by
      the Caqti type [ty], and is represented in a SQL query as [repr].

      For example, you might define the BOOL datatype as follows:
      {[
        let bool = Type.custom ~ty:Caqti_type.bool ~repr:"BOOLEAN"
      ]}

      {b Note} Petrol doesn't implement the boolean type using this
      function, and uses a slightly more efficient internal encoding,
      but for more bespoke custom user types this function should be
      sufficient. *)

  val pp_value : 'a t -> Format.formatter -> 'a -> unit
  (** [pp_value ty fmt vl] returns a pretty printer for values of a type [ty]. *)

  module Numeric : sig

    (** Poor man's type classes for numeric arguments. *)

    (** ['a integral] represents an integral type. *)
    type 'a integral =
      'a Type.Numeric.integral =
        Int : int integral
      | Int32 : int32 integral
      | Int64 : int64 integral

    (** ['a t] represents a numeric type. *)
    type 'a t =
      'a Type.Numeric.t =
        Int : int t
      | Int32 : int32 t
      | Int64 : int64 t
      | Real : float t
  end

end

type ('ret_ty, 'query_kind) query
(** [('ret_ty, 'query_tag) query] represents an SQL query that
    returns values of type ['ret_ty] and is a SQL query of kind
    ['query_kind] -- see {!Petrol.Query.t}. *)


type ('res, 'multiplicity) request
(** Represents a compiled SQL database request.  *)

module Request : sig

  (** This module defines a request type [t] that can be executed by
      Caqti (see {!exec}, {!find}, {!find_opt}). The functions defined
      in this module cache their inputs, so it is safe to call these
      repeatedly.

      {b Note} In order to cache a query, Petrol uses the string
      representation of the query with holes for variables as the
      cache key -- this means that you are highly recommended to {i
      not} use the static constant functions for any values that
      change frequently and instead use the non-static constant
      functions. *)

  type ('res, 'multiplicity) t = ('res, 'multiplicity) request
  (** Represents a compiled SQL database request.  *)

  val make_zero : (unit, 'b) query -> (unit, [ `Zero ]) t
  (** [make_zero query] constructs a SQL request with multiplicity
      zero from the query [query].  *)

  val make_one : ('a, 'b) query -> ('a, [ `One ]) t
  (** [make_one query] constructs a SQL request with multiplicity
      one from the query [query].  *)

  val make_zero_or_one : ('a, 'b) query -> ('a, [ `One | `Zero ]) t
  (** [make_one query] constructs a SQL request with multiplicity
      zero or one from the query [query].  *)

  val make_many : ('a, 'b) query -> ('a, [ `Many | `One | `Zero ]) t
  (** [make_one query] constructs a SQL request with multiplicity
      zero or more from the query [query].  *)

end


module Sqlite3 : sig

  module Request = Request

  (** Defines {!Petrol}'s e-DSL for Sqlite3 SQL.

      {b Note} Typically you should open this module at the start of the file.

      {[
        open Petrol
        open Petrol.Sqlite3
            Expr.[i 1; bl false]
        (* - : (int * (bool * ())) Expr.expr_list *)
      ]} *)


  module Type : sig

    (** Defines all supported Sqlite types. *)

    type 'a t = 'a Type.t
    (** Represents a SQL type. *)

    val bool : bool t
    (** [bool] represents the SQL boolean type (internally the type is INTEGER, as Sqlite does not have a dedicated boolean type). *)

    val int : int t
    (** [int] represents the SQL INTEGER type.  *)

    val real : float t
    (** [real] represents the SQL REAL type.  *)

    val text : string t
    (** [text] represents the SQL TEXT type.  *)

    val blob : string t
    (** [blob] represents the SQL BLOB type.  *)

    val null_ty : 'a t -> 'a option t

    module Numeric = Type.Numeric

  end 


  module Expr : sig

    (** Provides an SQL E-DSL for writing well-typed SQL expressions. *)

    type 'a t = 'a Expr.t
    (** ['a t] represents an SQL expression that produces a value
        corresponding to the type ['a].  *)

    (** Represents a heterogeneous sequence of SQL expressions.

        {b Note} Provided you have opened the Expr module, you can use
        List syntax to construct such lists:

        {[
          open Petrol.Sqlite3
              Expr.[i 1; bl false]
          (* - : (int * (bool * ())) Expr.expr_list *)
        ]}
    *)
    type 'a expr_list = 'a Expr.expr_list =
      | [] : unit expr_list
      | (::) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

    type wrapped_assign = Expr.wrapped_assign
    (** An opaque wrapper that represents an assignment of a value to a
        particular field in a table.  *)

    (** {1 Constants}*)

    (** The following functions define constant value expressions.

        {b Note} For each type, there are two flavours of constant
        expression: variable and static.

        The key difference between the two is in terms of how they are
        represented in the final SQL query - in particular, variable
        constant expressions are encoded as holes (?) in the query, to
        which a constant value is supplied, whereas static constant
        expressions are encoded directly in the query string.

        As Petrol functions cache the construction of SQL queries by
        their final string representation, you should prefer the dynamic
        form if you expect the constant value to change frequently - for
        example, if it is a constant value that you are receiving from
        elsewhere. Use the static form if you know that the value
        doesn't change and will always be the same.  *)

    val i : int -> int t
    (** [i v] returns an expression that evaluates to the integer value
        [v].  *)

    val f : float -> float t
    (** [f v] returns an expression that evaluates to the real value
        [v].  *)

    val s : string -> string t
    (** [s v] returns an expression that evaluates to the string value
        [v].  *)

    val i_opt : int option -> int option t
    val f_opt : float option -> float option t
    val s_opt : string option -> string option t
    val bl_opt : bool option -> bool option t

    val b : string -> string t
    (** [b v] returns an expression that evaluates to the bytes value
        [v].  *)

    val bl : bool -> bool t
    (** [bl v] returns an expression that evaluates to the bool value
        [v].  *)

    val vl: ty:'a Type.t -> 'a -> 'a t
    (** [vl ~ty value] returns an expression that evaluates to the value [value] with type [ty]. *)

    val i_stat : int -> int t
    (** [i_stat v] returns a static expression that evaluates to the
        integer value [v].  *)

    val f_stat : float -> float t
    (** [f_stat v] returns a static expression that evaluates to the
        real value [v].  *)

    val s_stat : string -> string t
    (** [s_stat v] returns a static expression that evaluates to the
        string value [v].  *)

    val b_stat : string -> string t
    (** [b_stat v] returns a static expression that evaluates to the
        bytes value [v].  *)

    val true_ : bool t
    (** [true_] represents the SQL constant [TRUE].  *)

    val false_ : bool t
    (** [false_] represents the SQL constant [FALSE].  *)

    val vl_stat: ty:'a Type.t -> 'a -> 'a t
    (** [vl_stat ~ty value] returns a static expression that evaluates
        to the value [value] with type [ty]. *)

    (** {1 Book-keeping: Types, Naming, Nulls}*)

    val as_ : 'a t -> name:string -> 'a t * 'a t
    (** [as_ exp ~name] returns a tuple [(exp',exp'_ref)] where [exp']
        is the SQL expression [exp AS name] that names [exp] as [name],
        and [exp'_ref] is simply [name]. *)

    val nullable: 'a t -> 'a option t
    (** [nullable e] encodes the fact that the expression [e] may return [NULL].  *)

    val is_not_null : 'a t -> bool t
    (** [is_not_null e] constructs an SQL expression that is [TRUE] iff
        the expression [e] is not [NULL] and [FALSE] otherwise.  *)

    val is_null : 'a t -> bool t
    (** [is_null e] constructs an SQL expression that is [TRUE] iff
        the expression [e] is [NULL] and [FALSE] otherwise.  *)

    val coerce : 'a t -> 'b Type.t -> 'b t
    (** [coerce expr ty] coerces expression [expr] to the type
        [ty]. This coercion is not checked, so make sure you know what
        you're doing or it could fail at runtime.  *)

    val (:=) : 'a t -> 'a t -> wrapped_assign
    (** [v := expr] returns an SQL expression that can be used with an
        update or insert clause to change the values in the database. *)

    val unset : 'a t -> wrapped_assign
    (** [unset v] returns an SQL expression that can be used with an
        update query to set a field to NULL in the database. *)

    (** {1 Operators} *)

    val ( + ) : int t -> int t -> int t
    val ( - ) : int t -> int t -> int t
    val ( * ) : int t -> int t -> int t
    val ( / ) : int t -> int t -> int t

    val ( +. ) : float t -> float t -> float t
    val ( -. ) : float t -> float t -> float t
    val ( *. ) : float t -> float t -> float t
    val ( /. ) : float t -> float t -> float t


    val ( = ) : 'a t -> 'a t -> bool t
    val ( <> ) : 'a t -> 'a t -> bool t
    val ( <= ) : 'a t -> 'a t -> bool t
    val ( < ) : 'a t -> 'a t -> bool t
    val ( > ) : 'a t -> 'a t -> bool t
    val ( >= ) : 'a t -> 'a t -> bool t
    val ( && ) : bool t -> bool t -> bool t
    val ( || ) : bool t -> bool t -> bool t
    val not : bool t -> bool t
    val exists: ('a, [> `SELECT | `SELECT_CORE ]) query -> bool t
    val in_ : 'a t -> ('a * unit, [> `SELECT | `SELECT_CORE]) query -> bool t

    val between : lower:'a t -> upper:'a t -> 'a t -> bool t
    val not_between : lower:'a t -> upper:'a t -> 'a t -> bool t

    (** {1 Functions} *)

    val count : ?distinct:bool -> 'a expr_list -> int t

    val count_star : int t

    val abs : int t -> int t

    val max : ?distinct:bool -> int t -> int t

    val min : ?distinct:bool -> int t -> int t

    val sum : ?distinct:bool -> int t -> int t

    val absf : float t -> float t

    val maxf : ?distinct:bool -> float t -> float t

    val minf : ?distinct:bool -> float t -> float t

    val sumf : ?distinct:bool -> float t -> float t

    val abs_gen : 'a Type.Numeric.t -> 'a t -> 'a t

    val max_gen : ?distinct:bool -> 'a Type.Numeric.t -> 'a t -> 'a t

    val min_gen : ?distinct:bool -> 'a Type.Numeric.t -> 'a t -> 'a t

    val sum_gen : ?distinct:bool -> 'a Type.Numeric.t -> 'a t -> 'a t

    val total : ?distinct:bool -> int t -> int t

    val group_concat: ?distinct:bool -> ?sep_by:string t -> string t -> string t

    val changes : int t

    val glob : pat:string t -> string t -> bool t

    val coalesce : 'a t list -> 'a t

    val like : string t -> pat:string t -> bool t

    val max_of : int t list -> int t

    val min_of : int t list -> int t

    val maxf_of : float t list -> float t

    val minf_of : float t list -> float t

    val max_gen_of : 'a Type.Numeric.t -> 'a t list -> 'a t

    val min_gen_of : 'a Type.Numeric.t -> 'a t list -> 'a t

    val random : int t

    val lower : string t -> string t

    val upper : string t -> string t

  end

end


module Postgres : sig

  module Request = Request

  (** Defines {!Petrol}'s e-DSL for Postgres SQL.

      {b Note} Typically you should open this module at the start of the file.

      {[
        open Petrol
        open Petrol.Postgres
            Expr.[i 1; bl false]
        (* - : (int * (bool * ())) Expr.expr_list *)
      ]} *)

  module Type : sig

    (** Defines all supported Postgres types. *)

    type 'a t = 'a Type.t
    (** Represents a SQL type. *)

    val bool : bool t
    (** [bool] represents the SQL boolean type. *)

    val int : int t
    (** [int] represents the SQL INTEGER type.  *)

    val real : float t
    (** [real] represents the SQL REAL type.  *)

    val text : string t
    (** [text] represents the SQL TEXT type.  *)

    val big_int : int64 Type.t
    (** [big_int] represents the SQL BIGINT type.  *)

    val big_serial : int64 Type.t
    (** [big_serial] represents the SQL BIGSERIAL type. *)

    val bytea : string Type.t
    (** [bytea] represents the SQL BYTEA type. *)

    val character : int -> string Type.t
    (** [character n] represents the SQL CHARACTER(n) type.  *)

    val character_varying : int -> string Type.t
    (** [character_varying n] represents the SQL CHARACTER VARYING(n) type.  *)

    val date : Ptime.t Type.t
    (** [date] represents the SQL DATE type.  *)

    val double_precision : float Type.t
    (** [double_precision] represents the SQL double_precision type. *)

    val int4 : int32 Type.t
    (** [int4] represents the SQL INT4 type. *)

    val smallint : int Type.t
    (** [smallint] represents the SQL SMALLINT type.  *)

    val smallserial : int Type.t
    (** [smallserial] represents the SQL SMALLSERIAL type. *)

    val time : Ptime.t Type.t
    (** [time] represents the SQL time type. *)

    module Numeric = Type.Numeric

  end 


  module Expr : sig

    (** Provides an SQL E-DSL for writing well-typed SQL expressions. *)

    type 'a t = 'a Expr.t
    (** ['a t] represents an SQL expression that produces a value
        corresponding to the type ['a].  *)

    (** Represents a heterogeneous sequence of SQL expressions.

        {b Note} Provided you have opened the Expr module, you can use
        List syntax to construct such lists:

        {[
          open Petrol.Sqlite3
              Expr.[i 1; bl false]
          (* - : (int * (bool * ())) Expr.expr_list *)
        ]}
    *)
    type 'a expr_list = 'a Expr.expr_list =
      | [] : unit expr_list
      | (::) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

    type wrapped_assign = Expr.wrapped_assign
    (** An opaque wrapper that represents an assignment of a value to a
        particular field in a table.  *)

    (** {1 Constants}*)

    (** The following functions define constant value expressions.

        {b Note} For each type, there are two flavours of constant
        expression: variable and static.

        The key difference between the two is in terms of how they are
        represented in the final SQL query - in particular, variable
        constant expressions are encoded as holes (?) in the query, to
        which a constant value is supplied, whereas static constant
        expressions are encoded directly in the query string.

        As Petrol functions cache the construction of SQL queries by
        their final string representation, you should prefer the dynamic
        form if you expect the constant value to change frequently - for
        example, if it is a constant value that you are receiving from
        elsewhere. Use the static form if you know that the value
        doesn't change and will always be the same.  *)

    val i : int -> int t
    (** [i v] returns an expression that evaluates to the integer value
        [v].  *)

    val f : float -> float t
    (** [f v] returns an expression that evaluates to the real value
        [v].  *)

    val s : string -> string t
    (** [s v] returns an expression that evaluates to the string value
        [v].  *)

    val i_opt : int option -> int option t
    val f_opt : float option -> float option t
    val s_opt : string option -> string option t
    val bl_opt : bool option -> bool option t

    val bl : bool -> bool t
    (** [bl v] returns an expression that evaluates to the bool value
        [v].  *)

    val vl: ty:'a Type.t -> 'a -> 'a t
    (** [vl ~ty value] returns an expression that evaluates to the value [value] with type [ty]. *)

    val i_stat : int -> int t
    (** [i_stat v] returns a static expression that evaluates to the
        integer value [v].  *)

    val f_stat : float -> float t
    (** [f_stat v] returns a static expression that evaluates to the
        real value [v].  *)

    val s_stat : string -> string t
    (** [s_stat v] returns a static expression that evaluates to the
        string value [v].  *)

    val true_ : bool t
    (** [true_] represents the SQL constant [TRUE].  *)

    val false_ : bool t
    (** [false_] represents the SQL constant [FALSE].  *)

    val vl_stat: ty:'a Type.t -> 'a -> 'a t
    (** [vl_stat ~ty value] returns a static expression that evaluates
        to the value [value] with type [ty]. *)

    (** {1 Book-keeping: Types, Naming, Nulls}*)

    val as_ : 'a t -> name:string -> 'a t * 'a t
    (** [as_ exp ~name] returns a tuple [(exp',exp'_ref)] where [exp']
        is the SQL expression [exp AS name] that names [exp] as [name],
        and [exp'_ref] is simply [name]. *)

    val nullable: 'a t -> 'a option t
    (** [nullable e] encodes the fact that the expression [e] may return [NULL].  *)

    val is_not_null : 'a t -> bool t
    (** [is_not_null e] constructs an SQL expression that is [TRUE] iff
        the expression [e] is not [NULL] and [FALSE] otherwise.  *)

    val is_null : 'a t -> bool t
    (** [is_null e] constructs an SQL expression that is [TRUE] iff
        the expression [e] is [NULL] and [FALSE] otherwise.  *)

    val coerce : 'a t -> 'b Type.t -> 'b t
    (** [coerce expr ty] coerces expression [expr] to the type
        [ty]. This coercion is not checked, so make sure you know what
        you're doing or it could fail at runtime.  *)

    val (:=) : 'a t -> 'a t -> wrapped_assign
    (** [v := expr] returns an SQL expression that can be used with an
        update or insert clause to change the values in the database. *)

    val unset : 'a t -> wrapped_assign
    (** [unset v] returns an SQL expression that can be used with an
        update query to set a field to NULL in the database. *)

    (** {1 Operators} *)

    val ( + ) : int t -> int t -> int t
    val ( - ) : int t -> int t -> int t
    val ( * ) : int t -> int t -> int t
    val ( / ) : int t -> int t -> int t

    val ( +. ) : float t -> float t -> float t
    val ( -. ) : float t -> float t -> float t
    val ( *. ) : float t -> float t -> float t
    val ( /. ) : float t -> float t -> float t


    val ( = ) : 'a t -> 'a t -> bool t
    val ( <> ) : 'a t -> 'a t -> bool t
    val ( <= ) : 'a t -> 'a t -> bool t
    val ( < ) : 'a t -> 'a t -> bool t
    val ( > ) : 'a t -> 'a t -> bool t
    val ( >= ) : 'a t -> 'a t -> bool t
    val ( && ) : bool t -> bool t -> bool t
    val ( || ) : bool t -> bool t -> bool t
    val not : bool t -> bool t
    val exists: ('a, [> `SELECT | `SELECT_CORE ]) query -> bool t
    val in_ : 'a t -> ('a * unit, [> `SELECT | `SELECT_CORE]) query -> bool t

    val between : lower:'a t -> upper:'a t -> 'a t -> bool t
    val not_between : lower:'a t -> upper:'a t -> 'a t -> bool t

    val between_symmetric : lower:'a t -> upper:'a t -> 'a t -> bool t
    val not_between_symmetric : lower:'a t -> upper:'a t -> 'a t -> bool t

    val is_distinct_from : 'a t -> 'a t -> bool t
    val is_not_distinct_from : 'a t -> 'a t -> bool t

    val is_true : bool t -> bool t
    val is_not_true : bool t -> bool t
    val is_false : bool t -> bool t
    val is_not_false : bool t -> bool t
    val is_unknown : bool t -> bool t
    val is_not_unknown : bool t -> bool t

    (** {1 Arithmetic Functions} *)

    val ceil : float t -> float t
    val floor : float t -> float t
    val round : float t -> float t
    val trunc : float t -> float t

    val ceili : int t -> int t
    val floori : int t -> int t
    val roundi : int t -> int t
    val trunci : int t -> int t

    val ceil_gen : ty:'a Type.Numeric.t -> 'a t -> 'a t
    val floor_gen : ty:'a Type.Numeric.t -> 'a t -> 'a t
    val round_gen : ty:'a Type.Numeric.t -> 'a t -> 'a t
    val trunc_gen : ty:'a Type.Numeric.t -> 'a t -> 'a t

    val pi : float t
    val sqrt : float t -> float t
    val degrees : float t -> float t
    val radians : float t -> float t
    val exp : float t -> float t
    val ln : float t -> float t
    val log10 : float t -> float t
    val log : base:float t -> float t -> float t

    val power : float t -> float t -> float t
    val poweri : int t -> int t -> int t
    val power_gen : ty:'a Type.Numeric.t -> 'a t -> 'a t -> 'a t

    val cos : float t -> float t
    val cosd : float t -> float t
    val acos : float t -> float t
    val acosd : float t -> float t
    val cosh : float t -> float t
    val acosh : float t -> float t

    val sin : float t -> float t
    val sind : float t -> float t
    val asin : float t -> float t
    val asind : float t -> float t
    val sinh : float t -> float t
    val asinh : float t -> float t

    val tan : float t -> float t
    val tand : float t -> float t
    val atan : float t -> float t
    val atand : float t -> float t
    val atan2 : float t -> float t
    val atan2d : float t -> float t
    val tanh : float t -> float t
    val atanh : float t -> float t

    val cot : float t -> float t
    val cotd : float t -> float t

    val factorial : int t -> int t
    val factorial_gen : ty:'a Type.Numeric.integral -> 'a t -> 'a t

    val gcd : int t -> int t -> int t
    val gcd_gen : ty:'a Type.Numeric.integral -> 'a t -> 'a t -> 'a t

    val lcm : int t -> int t -> int t
    val lcm_gen : ty:'a Type.Numeric.integral -> 'a t -> 'a t -> 'a t

    val abs : int t -> int t
    val absf : float t -> float t
    val abs_gen : 'a Type.Numeric.t -> 'a t -> 'a t

    (** {1 String functiosn}*)

    val concat : string t list -> string t

    val concat_ws : sep_by:string t -> string t list -> string t

    val like : string t -> pat:string t -> bool t

    val lower : string t -> string t

    val upper : string t -> string t

    val char_length : string t -> int t

    val length : string t -> int t

    val substring : ?from:int t -> ?for_:int t -> string t -> string t

    val replace : from:string t -> to_:string t -> string t -> string t

    val reverse : string t -> string t

    val starts_with : prefix:string t -> string t -> bool t

    val similar_to : pat:string t -> string t -> bool t

    (** {1 Aggregate Functions} *)

    val count : ?distinct:bool -> 'a expr_list -> int t

    val count_star : int t

    val coalesce : 'a t list -> 'a t

    val max : ?distinct:bool -> int t -> int t
    val maxf : ?distinct:bool -> float t -> float t
    val max_gen : ?distinct:bool -> 'a Type.Numeric.t -> 'a t -> 'a t

    val min : ?distinct:bool -> int t -> int t
    val minf : ?distinct:bool -> float t -> float t
    val min_gen : ?distinct:bool -> 'a Type.Numeric.t -> 'a t -> 'a t

    val sum : ?distinct:bool -> int t -> int t
    val sumf : ?distinct:bool -> float t -> float t
    val sum_gen : ?distinct:bool -> 'a Type.Numeric.t -> 'a t -> 'a t

    val greatest : int t list -> int t
    val greatestf : float t list -> float t
    val greatest_gen : ty:'a Type.Numeric.t -> 'a t list -> 'a t

    val least : int t list -> int t
    val leastf : float t list -> float t
    val least_gen : ty:'a Type.Numeric.t -> 'a t list -> 'a t

    (* val max_of : int t list -> int t *)

    (* val min_of : int t list -> int t *)

    (* val maxf_of : float t list -> float t *)

    (* val minf_of : float t list -> float t *)

    (* val max_gen_of : 'a Type.Numeric.t -> 'a t list -> 'a t *)

    (* val min_gen_of : 'a Type.Numeric.t -> 'a t list -> 'a t *)

    val random : float t

  end

end


module Schema : sig

  (** Provides an E-DSL for specifying SQL tables in OCaml.   *)

  type conflict_clause =
    [ `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ]
  type foreign_conflict_clause =
    [ `CASCADE | `NO_ACTION | `RESTRICT | `SET_DEFAULT | `SET_NULL ]

  type 'a constraint_
  (** ['a constraint_] represents a constraint on an SQL column or
      columns.  *)

  type 'a field
  (** ['a field] represents an SQL table field definition.  *)

  val field : ?constraints:[`Column] constraint_ list -> string -> ty:'a Type.t -> 'a field
  (** [field ?constraints name ~ty] constructs a new table column with
      name [name] and type [ty].

      [constraints] are a list of column constraints for the column.

      {b Note} [name] must be a valid SQL identifier - this is not
      checked by the function, but will cause an SQL error at
      runtime.  *)

  type 'a table =
    [] : unit table
  | (::) : ('a field * 'b table) -> ('a * 'b) table (** *)
  (** ['a table] represents a heterogeneous list of fields in a SQL
      Table schema, where ['a] captures the types of each element.

      Like {!Expr.expr_list}, if you have opened the Schema module,
      you can use vanilla list syntax to construct terms of this
      type.  *)

  val primary_key :
    ?name:string ->
    ?ordering:[ `ASC | `DESC ] ->
    ?on_conflict:conflict_clause ->
    ?auto_increment:bool -> unit -> [ `Column ] constraint_
  (** [primary_key ?name ?ordering ?on_conflict ?auto_increment ()]
      returns a new SQL column constraint that indicates that the
      column it is attached to must be the primary key.

      [name] is an optional name for the constraint for debugging
      purposes.

      [ordering] is the ordering of the primary key index.

      [on_conflict] specifies how to handle conflicts.

      [auto_increment] specifies whether the primary key should be
      automatically generated. (Note: not supported for Postgres
      databases.)  *)

  val table_primary_key :
    ?name:string ->
    ?on_conflict:conflict_clause -> string list -> [ `Table ] constraint_
  (** [table_primary_key ?name ?on_conflict cols] returns a new SQL
      table constraint that specifies that the table it is attached
      to's primary key is over the columns in [cols].

      [name] is an optional name for the constraint for debugging
      purposes.

      [on_conflict] specifies how to handle conflicts. *)

  val not_null :
    ?name:string ->
    ?on_conflict:conflict_clause -> unit -> [ `Column ] constraint_
  (** [not_null ?name ?on_conflict ()] returns a new SQL column
      constraint that specifies that the column it is attached to's
      value must not be NULL.

      [name] is an optional name for the constraint for debugging
      purposes.

      [on_conflict] specifies how to handle conflicts. *)

  val unique :
    ?name:string ->
    ?on_conflict:conflict_clause -> unit -> [ `Column ] constraint_
  (** [unique ?name ?on_conflict ()] returns a new SQL column
      constraint that specifies that the column it is attached to's
      values must be unique.

      [name] is an optional name for the constraint for debugging
      purposes.

      [on_conflict] specifies how to handle conflicts. *)

  val table_unique :
    ?name:string ->
    ?on_conflict:conflict_clause -> string list -> [ `Table ] constraint_
  (** [unique ?name ?on_conflict cols] returns a new SQL table
      constraint that specifies that the table it is attached to's
      values for the columns [cols] must be unique.

      [name] is an optional name for the constraint for debugging
      purposes.

      [on_conflict] specifies how to handle conflicts. *)

  val foreign_key :
    ?name:string ->
    ?on_update:foreign_conflict_clause ->
    ?on_delete:foreign_conflict_clause ->
    table:table_name ->
    columns:'a Expr.expr_list -> unit -> [ `Column ] constraint_
  (** [foreign_key ?name ?on_update ?on_delete ~table ~columns ()]
      returns a new SQL column constraint that specifies that the
      column it is attached to's values must be a foreign key into the
      table [table] with columns [columns].

      [name] is an optional name for the constraint for debugging
      purposes.

      [on_update] and [on_delete] specifies how to handle conflicts
      for updates and deletes respectively.  *)

  val table_foreign_key :
    ?name:string ->
    ?on_update:foreign_conflict_clause ->
    ?on_delete:foreign_conflict_clause ->
    table:table_name ->
    columns:'a Expr.expr_list -> string list -> [ `Table ] constraint_
    (** [table_foreign_key ?name ?on_update ?on_delete ~table ~columns
        cols] returns a new SQL table constraint that specifies that the
        table it is attached to's values for the columns [cols] must be
        a foreign key into the table [table] with columns [columns].

        [name] is an optional name for the constraint for debugging
        purposes.

        [on_update] and [on_delete] specifies how to handle conflicts
        for updates and deletes respectively..  *)

end

module Query : sig

  (** Provides an E-DSL for specifying SQL queries in OCaml.   *)

  type ('ret_ty, 'query_kind) t = ('ret_ty, 'query_kind) query
  (** [('ret_ty, 'query_tag) t] represents an SQL query that returns
      values of type ['ret_ty] and is a SQL query of kind
      ['query_kind].*)

  val pp : Format.formatter -> ('ret_ty, 'query_kind) t -> unit
  (** [pp fmt q] prints the query [q] in a form that can be parsed by
      an SQL engine. *)

  type join_op =
      LEFT                      (** LEFT join -- keep rows from the left table where the right column is NULL *)
    | RIGHT                     (** RIGHT join -- keep rows from the right table where the right column is NULL  *)
    | INNER                     (** INNER -- only keep rows for which both the left and right of the join are present. *)
  (** Defines the type of join to be used to combine two tables  *)

  type ('a, 'c) where_fun = bool Expr.t -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [< `DELETE | `SELECT | `SELECT_CORE | `UPDATE ]
  (** [('a,'c) where_fun] defines the type of an SQL function
      that corresponds to SQL's WHERE clause.  *)

  type ('a, 'b, 'c) group_by_fun =
    'b Expr.expr_list -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [< `SELECT | `SELECT_CORE ]
  (** [('a,'b,'c) group_by_fun] defines the type of an SQL function
      that corresponds to SQL's GROUP BY clause.  *)

  type ('a, 'c) having_fun = bool Expr.t -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [< `SELECT | `SELECT_CORE ]
  (** [('a,'b,'c) having_fun] defines the type of an SQL function
      that corresponds to SQL's HAVING clause.  *)

  type ('a, 'b, 'd, 'c) join_fun =
    ?op:join_op ->
    on:bool Expr.t -> ('b, 'd) t -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [< `SELECT_CORE ] constraint 'd = [< `SELECT_CORE | `SELECT | `TABLE]
  (** [('a,'b,'c,'d) join_fun] defines the type of an SQL function
      that corresponds to SQL's JOIN clause.  *)

  type ('a, 'b, 'c) on_err_fun = 'b -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [> `INSERT | `UPDATE ]
    constraint 'b = [< `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ]
  (** [('a,'b,'c) on_err] defines the type of an SQL function that
      corresponds to SQL's ON ERR clause for INSERT and UPDATE. (Note:
      only works for Sqlite3, use {!on_conflict} for portability when
      possible). *)

  type ('a, 'b, 'c) on_conflict_fun = 'b -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [> `INSERT ]
    constraint 'b = [< `DO_NOTHING ]
  (** [('a,'b,'c) on_conflict_fun] defines the type of an SQL function
      that corresponds to SQL's ON CONFLICT clause.  *)

  val select :
    'a Expr.expr_list -> from:table_name -> ('a, [> `SELECT_CORE ]) t
  (** [select fields ~from] corresponds to the SQL [SELECT {fields} FROM {from}]. *)

  val update :
    table:table_name -> set:Expr.wrapped_assign list -> (unit, [> `UPDATE ]) t
  (** [update ~table ~set] corresponds to the SQL [UPDATE {set} FROM {table}]. *)

  val insert :
    table:table_name ->
    values:Expr.wrapped_assign list -> (unit, [> `INSERT ]) t
  (** [insert ~table ~values] corresponds to the SQL [INSERT {values} INTO {table}]. *)

  val delete : from:table_name -> (unit, [> `DELETE ]) t
  (** [delete ~from] corresponds to the SQL [DELETE FROM {from}].  *)

  val where :
    ([< `DELETE | `SELECT | `SELECT_CORE | `UPDATE ], 'c) where_fun
  (** [where by expr] corresponds to the SQL [{expr} WHERE {by}].  *)

  val group_by : ([< `SELECT | `SELECT_CORE ], 'b, 'c) group_by_fun
  (** [group_by fields expr] corresponds to the SQL [{expr} GROUP BY {fields}].  *)

  val having : ([< `SELECT | `SELECT_CORE ], 'c) having_fun
  (** [having fields expr] corresponds to the SQL [{expr} HAVING {fields}].  *)

  val join : ([ `SELECT_CORE ], 'b, [< `SELECT_CORE | `SELECT | `TABLE], 'c) join_fun
  (** [join ?op ~on oexpr expr] corresponds to the SQL [{expr} {op} JOIN {oexpr} ON {expr}].

      The ordering of the last two arguments has been chosen to allow
      easily piping this with another SQL query. *)

  val on_err : ([< `INSERT | `UPDATE ], [ `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ], unit) on_err_fun
  (** [on_err err expr] corresponds to the SQL [{expr} ON ERR {err}].  *)

  val on_conflict : ([< `INSERT ], [ `DO_NOTHING ], unit) on_conflict_fun
  (** [on_conflict err expr] corresponds to the SQL [{expr} ON CONFLICT {err}] expression.  *)


  val limit :
    int Expr.t -> ('a, [< `SELECT | `SELECT_CORE ]) t -> ('a, [> `SELECT ]) t
  (** [limit count expr] corresponds to the SQL [{expr} LIMIT {count}].  *)

  val table : table_name -> (_, [> `TABLE]) t
  (** Make a table for a join  *)

  val offset
    : int Expr.t -> ('a, [< `SELECT | `SELECT_CORE ]) t -> ('a, [> `SELECT ]) t
  (** [offset count expr] corresponds to the SQL [{expr} OFFSET {fields}].  *)

  val order_by
    : ?direction:[ `ASC | `DESC ] -> 'a Expr.t -> ('b, [< `SELECT | `SELECT_CORE ]) t -> ('b, [> `SELECT ]) t
  (** [order_by ?direction fields expr] corresponds to the SQL [{expr}
      ORDER BY {direction} {fields}].  *)

  val order_by_
    : ?direction:[ `ASC | `DESC ] -> 'a Expr.expr_list
    -> ('b, [< `SELECT | `SELECT_CORE ]) t -> ('b, [> `SELECT ]) t
    (** [order_by_ ?direction fields expr] corresponds to the SQL
        [{expr} ORDER BY {direction} {fields}]. (In contrast to
        {!Petrol.Query.order_by}, this function allows passing a list
        of elements to be ordered by) *)

  val returning :
    'c Expr.expr_list -> ('a, [< `UPDATE | `INSERT | `DELETE] as 'b) t -> ('c, 'b) t
  (** [returning expr] corresponds to the SQL [RETURNING {expr}].

      The [RETURNING] clause is a non-standard extension supported by
      PostgreSQL since version 8.2 (2006-12-05), and by SQLite since version
      3.35.0 (2021-03-12). *)

end

module StaticSchema : sig

  (** Provides a helper interface, primarily for
      prototyping/debugging, that declares a static table without any
      versioning.  *)

  type t
  (** A global schema, primarily intended for testing.

      See also {!VersionedSchema.t}, which is the recommended
      alternative, especially if you expect the schema to change in
      the future.

      {b Note} A schema [t] here represents a collection of table
      schemas but doesn't have to be an exhaustive enumeration - i.e it is
      possible to have multiple [t] valid for a given SQL database
      provided they refer to disjoint collections of tables. *)

  val init: unit -> t
  (** [init version ~name] constructs a new schema. *)

  val declare_table : t ->
    ?constraints:[`Table] Schema.constraint_ list ->
    name:string -> 'a Schema.table -> table_name * 'a Expr.expr_list
  (** [declare_table t ?constraints ~name table_spec]
      declares a new table on the schema [t] with the name
      [name].

      [constraints] are a list of SQL constraints on the columns of
      the table. *)


  val initialise : t -> (module Caqti_lwt.CONNECTION) ->
    (unit, [> Caqti_error.t ]) Lwt_result.t
    (** [initialise t conn] initialises the SQL schema on [conn]. *)

end

module VersionedSchema : sig

  (** Provides an interface that declares a versioned schema. *)

  type t
  (** A versioned schema.

      {b Note} A schema [t] here represents a collection of table
      schemas but doesn't have to be an exhaustive enumeration - i.e it is
      possible to have multiple [t] valid for a given SQL database
      provided they refer to disjoint collections of tables. *)

  type version = private int list
  (** Lexiographically ordered schema version numbers  *)

  type migration = (unit, unit, [`Zero]) Caqti_request.t
  (** Represents SQL statements required to update the schema over
      versions. *)

  val version: int list -> version
  (** [version ls] constructs a new version number from [ls]. *)

  val init:
    ?migrations:(version * migration list) list -> version -> name:string -> t
  (** [init ?migrations dialect version ~name] constructs a new
      versioned schema declaring it to have the name [name] and
      version [version] using SQL dialect [dialect].

      [name] is the name of the schema -- used to initially
      determine the stored version number, and is required to stay
      constant over the lifetime of the project. 

      [version] is the current version of the schema -- note that
      {!initialise} will fail if it is run using an SQL database has a
      newer version than the version declared here.

      [migrations] is an association list, mapping versions to the SQL
      statements required to migrate the schema to the new format
      from its previous version. The order of elements in [migrations]
      is irrelevant. *)

  val declare_table : t ->
    ?since:version ->
    ?constraints:[`Table] Schema.constraint_ list ->
    ?migrations:(version * migration list) list ->
    name:string -> 'a Schema.table -> table_name * 'a Expr.expr_list
  (** [declare_table t ?since ?constraints ?migrations ~name table_spec]
      declares a new table on the schema [t] with the name
      [name].

      [since] declares the first version in which this table was
      introduced -- if omitted, Petrol assumes the table has been
      present since the very first version.

      [constraints] are a list of SQL constraints on the columns of
      the table.

      [migrations] is an association list, mapping versions to the SQL
      statements required to migrate this table to the new format
      from its previous version. The order of elements in [migrations]
      is irrelevant.  *)

  val migrations_needed : t -> (module Caqti_lwt.CONNECTION) ->
    (bool, [> Caqti_error.t  | `Newer_version_than_supported of version ]) Lwt_result.t
  (** [migrations_needed t conn] returns a boolean indicating whether
      the current version on the SQL database will require migrations
      -- i.e whether running {!initialise} will run migrations.

      [migrations_needed] will also fail if it is run using an SQL
      database has a newer version than the version declared here.

      {b Note} [migrations_needed] reserves the table name
      [petrol_<schema_name>_version_db] in the database. *)

  val initialise : t -> (module Caqti_lwt.CONNECTION) ->
    (unit, [> Caqti_error.t | `Newer_version_than_supported of version ]) Lwt_result.t
    (** [initialise t conn] initialises the SQL database on [conn],
        performing any necessary migrations if needed.

        [initialise] will fail if it is run using an SQL database has a
        newer version than the version declared here.

        {b Note} [initialise] reserves the table name
        [petrol_<schema_name>_version_db] in the database.  *)

end

val exec : (module Caqti_lwt.CONNECTION) -> (unit, [< `Zero ]) request ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t
(** [exec db req] executes a unit SQL request [req] on the SQL
    database [db].  *)

val find : (module Caqti_lwt.CONNECTION) -> ('a, [< `One ]) request ->
  ('a, [> Caqti_error.call_or_retrieve ]) result Lwt.t
(** [find db req] executes a singleton SQL request [req] on the SQL
    database [db] returning the result.  *)

val find_opt : (module Caqti_lwt.CONNECTION) ->
  ('a, [< `One | `Zero ]) request ->
  ('a option, [> Caqti_error.call_or_retrieve ]) result Lwt.t
(** [find_opt db req] executes a zero-or-one SQL request [req] on the SQL
    database [db] returning the result if it exists.  *)

val collect_list :
  (module Caqti_lwt.CONNECTION) ->
  ('a, [< `Many | `One | `Zero ]) request ->
  ('a list, [> Caqti_error.call_or_retrieve ]) result Lwt.t
(** [collect_list db req] executes a SQL request [req] on the SQL
    database [db] and collects the results into a list.  *)
