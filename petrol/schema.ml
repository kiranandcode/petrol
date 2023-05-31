type table_name = Types.table_name

type conflict_clause = [`ROLLBACK | `ABORT | `FAIL | `IGNORE | `REPLACE]
type foreign_conflict_clause = [`SET_NULL | `SET_DEFAULT | `CASCADE | `RESTRICT | `NO_ACTION ]

type 'a sql_constraint =
  | PrimaryKey of {
    name: string option;
    ordering: [`ASC | `DESC] option;
    local_columns: string list option;
    on_conflict: conflict_clause option;
    auto_increment: bool
  }
  | NotNull of {
      name: string option;
      on_conflict: conflict_clause option
    }
  | Unique of {
      name: string option;
      local_columns: string list option;
      on_conflict: conflict_clause option;
    }
  | ForeignKey of {
      local_columns: string list option;
      name: string option;
      table: Types.table_name;
      columns: string list;
      on_update: foreign_conflict_clause option;
      on_delete: foreign_conflict_clause option;
    }

type 'a field = string * 'a Type.t * [`Column] sql_constraint list
type 'a constraint_ = 'a sql_constraint

let pp_conflict_clause fmt = function
  | `ROLLBACK ->
    Format.fprintf fmt "ON CONFLICT ROLLBACK"
  | `ABORT ->
    Format.fprintf fmt "ON CONFLICT ABORT"
  | `FAIL ->
    Format.fprintf fmt "ON CONFLICT FAIL"
  | `IGNORE ->
    Format.fprintf fmt "ON CONFLICT IGNORE"
  | `REPLACE ->
    Format.fprintf fmt "ON CONFLICT REPLACE"


let pp_foreign_conflict_clause fmt = function
  | `SET_NULL -> Format.fprintf fmt "SET NULL"
  | `SET_DEFAULT -> Format.fprintf fmt "SET DEFAULT"
  | `CASCADE -> Format.fprintf fmt "CASCADE"
  | `RESTRICT -> Format.fprintf fmt "RESTRICT"
  | `NO_ACTION -> Format.fprintf fmt "NO ACTION"


let pp_opt f fmt = function
    None -> ()
  | Some v -> Format.fprintf fmt " %a" f v
let pp_parens f fmt = fun v ->
  Format.fprintf fmt "(%a)" f v

let pp_ordering fmt = function
  | `ASC -> Format.fprintf fmt "ASC"
  | `DESC -> Format.fprintf fmt "DESC"

let pp_column_list fmt ls =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
    Format.pp_print_string fmt ls

let pp_constraint_name fmt = function
  | None -> ()
  | Some name ->
    Format.fprintf fmt "CONSTRAINT %s " name

let pp_sql_constraint fmt = function
  | PrimaryKey {
    name; ordering; local_columns;
    on_conflict; auto_increment;
  } ->
    Format.fprintf fmt
      "%aPRIMARY KEY%a%a%a%a"
      pp_constraint_name name
      (pp_opt pp_ordering) ordering
      (pp_opt (pp_parens pp_column_list)) local_columns
      (pp_opt pp_conflict_clause) on_conflict
      (fun fmt vl ->
         if vl then
           Format.fprintf fmt " AUTOINCREMENT")
      auto_increment
  | NotNull { name; on_conflict } ->
    Format.fprintf fmt
      "%aNOT NULL%a"
      pp_constraint_name name
      (pp_opt pp_conflict_clause) on_conflict
  | Unique { name; local_columns; on_conflict } ->
    Format.fprintf fmt
      "%aUNIQUE%a%a"
      pp_constraint_name name
      (pp_opt (pp_parens pp_column_list)) local_columns
      (pp_opt pp_conflict_clause) on_conflict
  | ForeignKey {
    local_columns;
    name;
    table;
    columns;
    on_update;
    on_delete
  } ->
    Format.fprintf fmt
      "%a%a%sREFERENCES %s %a%a%a"
      pp_constraint_name name
      (pp_opt (fun fmt vl ->
         Format.fprintf fmt
           "FOREIGN KEY %a"
           (pp_parens pp_column_list)
           vl)) local_columns
      (if Option.is_some name || Option.is_some local_columns then " " else "")
      (snd table)
      (pp_parens pp_column_list) columns
      (pp_opt (fun fmt vl ->
         Format.fprintf fmt "ON UPDATE %a"
           pp_foreign_conflict_clause vl)) on_update
      (pp_opt (fun fmt vl ->
         Format.fprintf fmt "ON DELETE %a"
           pp_foreign_conflict_clause vl)) on_delete

let ensure_table_constraint : 'a sql_constraint -> unit =
  function
  | NotNull _ ->
    invalid_arg "NOT NULL constraints are not table constraints"
  | PrimaryKey {
    name=_; ordering; local_columns; on_conflict=_;
    auto_increment } ->
    if auto_increment then
      invalid_arg "PRIMARY KEY constraints when given as a table \
                   constraint can not auto-increment";
    if Option.is_some ordering then
      invalid_arg "PRIMARY KEY constraints when given as a table \
                   constraint can not specify ordering";
    if Option.is_none local_columns then
      invalid_arg "PRIMARY KEY constraints when given as a table \
                   constraint must specify columns explicitly";
  | Unique { name=_; local_columns; on_conflict=_ } ->
    if Option.is_none local_columns then
      invalid_arg "UNIQUE constraints when given as a table \
                   constraint must specify columns explicitly";
  | ForeignKey { local_columns; name=_; table=_; columns=_;
                 on_update=_; on_delete=_ } ->
    if Option.is_none local_columns then
      invalid_arg "UNIQUE constraints when given as a table \
                   constraint must specify the local columns \
                   explicitly"

let ensure_column_constraint : 'a sql_constraint -> unit =
  function
  | NotNull _ -> ()
  | PrimaryKey {
    name=_; ordering=_; local_columns; on_conflict=_;
    auto_increment=_ } ->
    if Option.is_some local_columns then
      invalid_arg "PRIMARY KEY column constraints can not list \
                   columns explicitly";
  | Unique { name=_; local_columns; on_conflict=_ } ->
    if Option.is_some local_columns then
      invalid_arg "UNIQUE column constraints can not list \
                   columns explicitly";
  | ForeignKey { local_columns; name=_; table=_; columns=_;
                 on_update=_; on_delete=_ } ->
    if Option.is_some local_columns then
      invalid_arg "FOREIGN KEY column constraints can not list \
                   columns explicitly"

let field ?(constraints : _ list =[]) name ~ty : 'a field =
  List.iter ensure_column_constraint constraints;
  (name, ty, constraints)

let field_name = function (name, _, _) -> name

let primary_key
      ?name ?ordering ?on_conflict
      ?(auto_increment=false) () : [`Column] sql_constraint =
  PrimaryKey {
    name;
    ordering;
    local_columns=None;
    on_conflict; auto_increment
  }

let table_primary_key
      ?name ?on_conflict
      columns : [`Table] sql_constraint =
  PrimaryKey {
    name;
    ordering=None;
    local_columns=Some columns;
    on_conflict;
    auto_increment=false
  }

let not_null ?name ?on_conflict () : [`Column] sql_constraint =
  NotNull { name; on_conflict }

let unique ?name ?on_conflict () : [`Column] sql_constraint =
  Unique {
    name;
    local_columns=None;
    on_conflict;
  }

let table_unique ?name ?on_conflict columns : [`Table] sql_constraint =
  Unique {
    name;
    local_columns=Some columns;
    on_conflict;
  }

let rec expr_list_to_column_names : 'a . Types.table_name -> 'a Expr.expr_list -> string list =
  fun (type a) table_name (ls: a Types.expr_list) : string list ->
  match ls with
  | [] -> []
  | Types.FIELD (table_name', name, _) :: t ->
    if not (table_name = table_name') then
      invalid_arg "foreign key constraint uses fields from a \
                   different table than the one specified";
    name :: expr_list_to_column_names table_name t
  | _ :: _ ->
    invalid_arg "foreign key constraint must operate on fields \
                 directly not derived expressions" 

let foreign_key ?name ?on_update ?on_delete ~table ~columns () : [`Column] sql_constraint =
  ForeignKey {
    local_columns=None;
    name;
    table;
    columns=expr_list_to_column_names table columns;
    on_update;
    on_delete;
  }

let table_foreign_key ?name ?on_update ?on_delete ~table ~columns local_columns : [`Table] sql_constraint =
  ForeignKey {
    local_columns=Some local_columns;
    name;
    table;
    columns=expr_list_to_column_names table columns;
    on_update;
    on_delete;
  }


let ty : 'a field -> 'a Type.t = function (_, ty, _) -> ty

type 'a table =
  | [] : unit table
  | (::) : ('a field * 'b table) -> ('a * 'b) table

let to_sql ~name table (constraints: 'a sql_constraint list) = 
  let rec loop  : 'a . string option -> 'a table -> string option = fun acc (type a) (table: a table) ->
    let update_acc_with acc res =
      let acc = match acc with None -> "" | Some v -> v ^ ", "  in
      let acc = acc ^ res in
      Some acc in
    match table with
    | [] -> acc
    | ((f, ty, constraints) :: rest) ->
      let constraints_text =
        match constraints with
          [] -> ""
        | _ :: _ ->
          Format.asprintf " %a"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_sql_constraint)
            constraints in
      let acc = update_acc_with acc @@ f ^ " " ^ Type.show ty ^ constraints_text in
      loop acc rest in
  let acc = (loop None table) in
  let acc = match acc,constraints with
    | None, [] -> ""
    | Some acc, [] -> acc
    | None, constraints ->
      Format.asprintf "%a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp_sql_constraint) constraints
    | Some acc, constraints ->
      Format.asprintf "%s, %a" acc
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp_sql_constraint) constraints in
  Format.sprintf "CREATE TABLE IF NOT EXISTS %s (%s)"
    name acc

