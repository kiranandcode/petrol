type table_name
(** Uniquely identifies a table in the system.  *)

module Type : sig

  (** Defines all supported SQL types. *)

  type 'a t = 'a Type.t
  (** Represents a SQL type. *)

  val bool : bool t
  (** [bool] represents the SQL boolean type (or INTEGER if BOOL does not exist). *)

  val int : int t
  (** [int] represents the SQL INTEGER type.  *)

  val real : float t
  (** [real] represents the SQL REAL type.  *)

  val text : string t
  (** [text] represents the SQL TEXT type.  *)

  val blob : string t
  (** [blob] represents the SQL BLOB type.  *)

end 


module Expr : sig

  (** Provides an SQL E-DSL for writing well-typed SQL expressions. *)

  type 'a t
  (** ['a t] represents an SQL expression that produces a value
      corresponding to the type ['a].  *)

  type 'a expr_list =
    | [] : unit expr_list
    | (::) : ('a t * 'b expr_list) -> ('a * 'b) expr_list (** *)
  (** Represents a heterogeneous sequence of SQL expressions.

      {b Note} Provided you have opened the Expr module, you can use
      List syntax to construct such lists:

      {[
        Petrol.Expr.[i 1; bl false]
        (* - : (int * (bool * ())) Petrol.Expr.expr_list *)
      ]}
  *)

  val pp: Format.formatter -> 'a t -> unit
  (** [pp fmt expr] pretty prints an SQL expression as a string that
      can be parsed by an SQL engine.  *)

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
      doesn't change and will always be the same value.  *)

  val i : int -> int t
  (** [i v] returns an expression that evaluates to the integer value
      [v].  *)

  val f : float -> float t
  (** [f v] returns an expression that evaluates to the real value
      [v].  *)

  val s : string -> string t
  (** [s v] returns an expression that evaluates to the string value
      [v].  *)

  val b : string -> string t
  (** [b v] returns an expression that evaluates to the bytes value
      [v].  *)

  val bl : bool -> bool t
  (** [bl v] returns an expression that evaluates to the bool value
      [v].  *)

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

  val coerce : 'a t -> 'b Type.t -> 'b t
  (** [coerce expr ty] coerces expression [expr] to the type
      [ty]. This coercion is not checked, so make sure you know what
      you're doing or it could fail at runtime.  *)

  (** {1 Operators} *)

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

  (** {1 Functions} *)

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
      automatically generated.
  *)

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

  type ('ret_ty, 'query_kind) t
  (** [('ret_ty, 'query_tag) t] represents an SQL query that returns
      values of type ['ret_ty] and is a SQL query of kind
      ['query_kind].*)

  type join_op =
      LEFT                      (** LEFT join -- keep rows from the left table where the right column is NULL *)
    | RIGHT                     (** RIGHT join -- keep rows from the right table where the right column is NULL  *)
    | INNER                     (** INNER -- only keep rows for which both the left and right of the join are present. *)
  (** Defines the type of join to be used to combine two tables  *)

  type wrapped_assign
  (** An opaque wrapper that represents an assignment of a value to a
      particular field in a table.  *)

  type ('a, 'c) filter_fun = bool Expr.t -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [< `DELETE | `SELECT | `SELECT_CORE | `UPDATE ]
  (** [('a,'c) filter_fun] defines the type of an SQL function
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
    constraint 'a = [< `SELECT_CORE ] constraint 'd = [< `SELECT_CORE | `SELECT ]
  (** [('a,'b,'c,'d) join_fun] defines the type of an SQL function
      that corresponds to SQL's JOIN clause.  *)

    type ('a, 'b, 'c) on_err_fun = 'b -> ('c, 'a) t -> ('c, 'a) t
    constraint 'a = [> `INSERT | `UPDATE ]
    constraint 'b = [< `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ]
  (** [('a,'b,'c) having_fun] defines the type of an SQL function
      that corresponds to SQL's HAVING clause ON ERR.  *)

  val (:=) : 'a Expr.t -> 'a Expr.t -> wrapped_assign
  (** [v := expr] returns an SQL expression that can be used with an
      update or insert clause to change the values in the database. *)

  val select :
    'a Expr.expr_list -> from:table_name -> ('a, [> `SELECT_CORE ]) t
  (** [select fields ~from] corresponds to the SQL [SELECT {fields} FROM {from}]. *)

  val update :
    table:table_name -> set:wrapped_assign list -> (unit, [> `UPDATE ]) t
  (** [update ~table ~set] corresponds to the SQL [UPDATE {set} FROM {table}]. *)

  val insert :
    table:table_name ->
    values:wrapped_assign list -> (unit, [> `INSERT ]) t
  (** [insert ~table ~values] corresponds to the SQL [INSERT {values} INTO {table}]. *)

  val delete : from:table_name -> (unit, [> `DELETE ]) t
  (** [delete ~from] corresponds to the SQL [DELETE FROM {from}].  *)

  val filter :
    ([< `DELETE | `SELECT | `SELECT_CORE | `UPDATE ], 'c) filter_fun
  (** [filter by expr] corresponds to the SQL [{expr} WHERE {by}].  *)

  val group_by : ([< `SELECT | `SELECT_CORE ], 'b, 'c) group_by_fun
  (** [group_by fields expr] corresponds to the SQL [{expr} GROUP BY {fields}].  *)

  val having : ([< `SELECT | `SELECT_CORE ], 'c) having_fun
  (** [having fields expr] corresponds to the SQL [{expr} HAVING {fields}].  *)

  val join : ([ `SELECT_CORE ], 'b, [< `SELECT_CORE | `SELECT ], 'c) join_fun
  (** [join ?op ~on oexpr expr] corresponds to the SQL [{expr} {op} JOIN {oexpr} ON {expr}].

  The ordering of the last two arguments has been chosen to allow
  easily piping this with another SQL query. *)

  val on_err : [ `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ] ->
    (unit, 'a) t -> (unit, 'a) t
  (** [on_err err expr] corresponds to the SQL [{expr} ON ERR {err}].  *)

  val limit :
    int Expr.t -> ('a, [ `SELECT | `SELECT_CORE ]) t -> ('a, [> `SELECT ]) t
  (** [limit count expr] corresponds to the SQL [{expr} LIMIT {count}].  *)

  val offset
    : int Expr.t -> ('a, [ `SELECT | `SELECT_CORE ]) t -> ('a, [> `SELECT ]) t
  (** [offset count expr] corresponds to the SQL [{expr} OFFSET {fields}].  *)

  val order_by
    : ?direction:[ `ASC | `DESC ] -> 'a Expr.t -> ('b, [ `SELECT | `SELECT_CORE ]) t -> ('b, [> `SELECT ]) t
  (** [order_by ?direction fields expr] corresponds to the SQL [{expr}
      ORDER BY {direction} {fields}].  *)

end

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

  type ('res, 'multiplicity) t
  (** Represents a compiled SQL database request.  *)

  val make_zero : (unit, 'b) Query.t -> (unit, [ `Zero ]) t
  (** [make_zero query] constructs a SQL request with multiplicity
      zero from the query [query].  *)

  val make_one : ('a, 'b) Query.t -> ('a, [ `One ]) t
  (** [make_one query] constructs a SQL request with multiplicity
      one from the query [query].  *)

  val make_zero_or_one : ('a, 'b) Query.t -> ('a, [ `One | `Zero ]) t
  (** [make_one query] constructs a SQL request with multiplicity
      zero or one from the query [query].  *)

  val make_many : ('a, 'b) Query.t -> ('a, [ `Many | `One | `Zero ]) t
  (** [make_one query] constructs a SQL request with multiplicity
      zero or more from the query [query].  *)

end

val exec : (module Caqti_lwt.CONNECTION) -> (unit, [< `Zero ]) Request.t ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t
(** [exec db req] executes a unit SQL request [req] on the SQL
    database [db].  *)

val find : (module Caqti_lwt.CONNECTION) -> ('a, [< `One ]) Request.t ->
  ('a, [> Caqti_error.call_or_retrieve ]) result Lwt.t
(** [find db req] executes a singleton SQL request [req] on the SQL
    database [db] returning the result.  *)

val find_opt : (module Caqti_lwt.CONNECTION) ->
  ('a, [< `One | `Zero ]) Request.t ->
  ('a option, [> Caqti_error.call_or_retrieve ]) result Lwt.t
(** [find_opt db req] executes a zero-or-one SQL request [req] on the SQL
    database [db] returning the result if it exists.  *)

val collect_list :
  (module Caqti_lwt.CONNECTION) ->
  ('a, [< `Many | `One | `Zero ]) Request.t ->
  ('a list, [> Caqti_error.call_or_retrieve ]) result Lwt.t
(** [collect_list db req] executes a SQL request [req] on the SQL
    database [db] and collects the results into a list.  *)

module GlobalDatabase : sig

  (** Provides a helper interface, primarily for
      prototyping/debugging, that declares a single global table
      without any versioning.  *)

  type t
  (** A global database, primarily intended for testing.

      See also {!VersionedDatabase.t}, which is the recommended
      alternative, especially if you expect the schema to change in
      the future.

  {b Note} A database [t] here represents a collection of table
  schemas but doesn't have to be an exhaustive enumeration - i.e it is
  possible to have multiple [t] valid for a given SQL database
  provided they refer to disjoint collections of tables. *)

  val init: unit -> t
  (** [init version ~name] constructs a new database. *)

  val declare_table : t ->
    ?constraints:[`Table] Schema.constraint_ list ->
    name:string -> 'a Schema.table -> Types.table_name * 'a Expr.expr_list
  (** [declare_table t ?constraints ~name table_spec]
      declares a new table on the database [t] with the name
      [name].

      [constraints] are a list of SQL constraints on the columns of
      the table. *)


  val initialise : t -> (module Caqti_lwt.CONNECTION) ->
    (unit, [> `Msg of string ]) Lwt_result.t
  (** [initialise t conn] initialises the SQL database on [conn]. *)

end

module VersionedDatabase : sig

  (** Provides an interface that declares a versioned database
      schema. *)

  type t
  (** A versioned database.

  {b Note} A database [t] here represents a collection of table
  schemas but doesn't have to be an exhaustive enumeration - i.e it is
  possible to have multiple [t] valid for a given SQL database
  provided they refer to disjoint collections of tables. *)

  type version = private int list
  (** Lexiographically ordered database version numbers  *)

  type migration = (unit, unit, [`Zero]) Caqti_request.t
  (** Represents SQL statements required to update the database schema
      over versions. *)

  val version: int list -> version
  (** [version ls] constructs a new version number from [ls]. *)

  val init:
    ?migrations:(version * migration list) list -> version -> name:string -> t
  (** [init ?migrations version ~name] constructs a new versioned
      database declaring it to have the name [name] and version
      [version].

      [name] is the name of the database -- used to initially
      determine the stored version number, and is required to stay
      constant over the lifetime of the project.

      [version] is the current version of the database -- note that
      {!initialise} will fail if it is run using an SQL database has a
      newer version than the version declared here.

      [migrations] is an association list, mapping versions to the SQL
      statements required to migrate the database to the new format
      from its previous version. The order of elements in [migrations]
      is irrelevant. *)

  val declare_table : t ->
    ?constraints:[`Table] Schema.constraint_ list ->
    ?migrations:(version * migration) list ->
    name:string -> 'a Schema.table -> Types.table_name * 'a Expr.expr_list
  (** [declare_table t ?constraints ?migrations ~name table_spec]
      declares a new table on the database [t] with the name
      [name].

      [constraints] are a list of SQL constraints on the columns of
      the table.

      [migrations] is an association list, mapping versions to the SQL
      statements required to migrate this table to the new format
      from its previous version. The order of elements in [migrations]
      is irrelevant.  *)

  val migrations_needed : t -> (module Caqti_lwt.CONNECTION) ->
    (bool, [> `Msg of string ]) Lwt_result.t
  (** [migrations_needed t conn] returns a boolean indicating whether
      the current version on the SQL database will require migrations
      -- i.e whether running {!initialise} will run migrations.

      [migrations_needed] will also fail if it is run using an SQL
      database has a newer version than the version declared here.  *)

  val initialise : t -> (module Caqti_lwt.CONNECTION) ->
    (unit, [> `Msg of string ]) Lwt_result.t
  (** [initialise t conn] initialises the SQL database on [conn],
      performing any necessary migrations if needed.

      [initialise] will fail if it is run using an SQL database has a
      newer version than the version declared here.  *)

end
