type ('y, 'ty) t = ('y, 'ty) Types.query

type join_op = Types.join_op = LEFT | RIGHT | INNER

type ('a,'c) filter_fun =
  bool Expr.t -> ('c, 'a) t
  -> ('c, 'a) t
  constraint 'a =
    ([< `SELECT_CORE | `SELECT | `DELETE | `UPDATE]) as 'a

type ('a,'b,'c) group_by_fun =
  'b Expr.expr_list -> ('c, 'a) t -> ('c, 'a) t constraint 'a = ([< `SELECT_CORE | `SELECT ] as 'a)

type ('a,'c) having_fun =
  bool Expr.t -> ('c, 'a) t
  -> ('c, 'a) t
  constraint 'a =
    ([< `SELECT_CORE | `SELECT ]) as 'a

type ('a,'b,'d,'c) join_fun =
  ?op:Types.join_op -> on:bool Expr.t ->
  ('b, [< `SELECT_CORE | `SELECT ] as 'd) t
  -> ('c, 'a) t -> ('c, 'a) t
  constraint 'a = ([< `SELECT_CORE]) as 'a

type ('a,'b,'c) on_err_fun =
  ([< `ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ] as 'b) ->
  ('c, 'a) t
  -> ('c, 'a) t
  constraint 'a = ([> `UPDATE | `INSERT]) as 'a

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

let rec query_values : 'a 'b. Expr.wrapped_value list -> ('a,'b) t -> Expr.wrapped_value list =
  fun acc (type a b) (query: (a,b) t) ->
  match query with
  | SELECT_CORE { exprs; table=_; join; where; group_by; having } ->
    let acc = Expr.values_expr_list acc exprs in
    let acc = List.fold_left (fun acc (Types.MkJoin {table; on; join_op=_}) ->
      let acc = query_values acc table in
      let acc = Expr.values acc on in
      acc
    ) acc join in
    let acc = Option.map (Expr.values acc) where |> Option.value ~default:acc in
    let acc = Option.map (Expr.values_expr_list acc) group_by |> Option.value ~default:acc in
    let acc = Option.map (Expr.values acc) having |> Option.value ~default:acc in
    acc
  | SELECT { core; order_by; limit; offset } ->
    let acc = query_values acc core in
    let acc = Option.map (fun (_, expr) -> Expr.values acc expr) order_by
              |> Option.value ~default:acc in
    let acc = Option.map (Expr.values acc) limit |> Option.value ~default:acc in
    let acc = Option.map (Expr.values acc) offset |> Option.value ~default:acc in
    acc
  | DELETE { table=_; where } ->
    let acc = Option.map (Expr.values acc) where |> Option.value ~default:acc in
    acc
  | UPDATE { table=_; on_err=_; set; where } ->
    let acc = List.fold_left (fun acc (Types.ASSIGN (vl, expr)) ->
      Expr.values (Expr.values acc vl) expr) acc set in
    let acc = Option.map (Expr.values acc) where |> Option.value ~default:acc in
    acc
  | INSERT { table=_; on_err=_; set } ->
    let acc = List.fold_left (fun acc (Types.ASSIGN (vl, _)) ->
      Expr.values acc vl) acc set in
    let acc = List.fold_left (fun acc (Types.ASSIGN (_, expr)) ->
      Expr.values acc expr) acc set in
    acc

let query_values query = List.rev (query_values [] query)

let rec pp_query: 'a 'b. Format.formatter ->
  ('a, 'b) t -> unit =
  fun fmt (type a b) (query: (a,b) t) ->
  (match query with
   | SELECT_CORE { exprs; table; join; where; group_by; having } ->
     Format.fprintf fmt
       "SELECT %a\nFROM %s%a%a%a%a"
       Expr.pp_expr_list exprs
       (snd table)
       pp_join_list join
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "WHERE %a" Expr.pp vl))
       where
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "GROUP BY %a"
            Expr.pp_expr_list vl))
       group_by
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "HAVING %a"
            Expr.pp vl))
       having
   | SELECT { core; order_by; limit; offset } ->
     Format.fprintf fmt "%a%a%a%a"
       pp_query core
       (pp_opt (fun fmt (order,vl) ->
          Format.fprintf fmt "ORDER BY %a %a"
            Expr.pp vl pp_ordering order))
       order_by
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "LIMIT %a" Expr.pp vl))
       limit
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "OFFSET %a" Expr.pp vl))
       offset
   | DELETE { table; where } ->
     Format.fprintf fmt "DELETE FROM %s%a"
       (snd table)
       (pp_opt (fun fmt vl ->
          Format.fprintf fmt "WHERE %a"
            Expr.pp vl
        ))
       where
   | UPDATE { table; on_err; set; where } ->
     Format.fprintf fmt "UPDATE%a %s\nSET %a%a"
       (pp_opt pp_on_err) on_err
       (snd table)
       (Format.pp_print_list ~pp_sep:(fun fmt () ->
          Format.fprintf fmt ", ") pp_wrapped_assign) set
       (pp_opt (fun fmt vl -> Format.fprintf fmt "WHERE %a" Expr.pp vl))
       where
   | INSERT { table; on_err; set } ->
     let pp_field : 'a . Format.formatter -> 'a Expr.t -> unit =
       fun fmt (type a) (expr: a Expr.t) : unit ->
         match expr with
         | FIELD (_, field, _) -> Format.fprintf fmt "%s" field
         | _ -> Format.kasprintf failwith "expected field for INSERT query, got %a" Expr.pp expr in
     Format.fprintf fmt "INSERT%a INTO %s (%a) VALUES (%a)"
       (pp_opt pp_on_err) on_err
       (snd table)
       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
          (fun fmt (Types.ASSIGN (fld, _)) -> Format.fprintf fmt "%a" pp_field fld))
       set
       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
          (fun fmt (Types.ASSIGN (_, expr)) -> Format.fprintf fmt "%a" Expr.pp expr))
       set)
and pp_join : Format.formatter -> Types.join -> unit =
  fun fmt (MkJoin { table; on; join_op }) ->
  Format.fprintf fmt "%a (%a) ON %a"
    Types.pp_join_op join_op
    pp_query table
    Expr.pp on
and pp_join_list : Format.formatter -> Types.join list -> unit =
  fun fmt ls ->
  match ls with
  | [] -> ()
  | h :: t ->
    Format.fprintf fmt " %a%a" pp_join h pp_join_list t
and pp_wrapped_assign: Format.formatter -> Types.wrapped_assign -> unit =
  fun fmt (ASSIGN (vl, expr)) ->
  Format.fprintf fmt "%a = %a" Expr.pp vl Expr.pp expr

let show_query q = Format.asprintf "%a" pp_query q


let query_ret_ty: 'a 'b. ('a,'b) t -> 'a Type.ty_list =
  fun (type a b) (query: (a,b) t) : a Type.ty_list ->
  match query with
  | SELECT_CORE { exprs; table=_; join=_; where=_; group_by=_; having=_ } ->
    Expr.ty_expr_list exprs
  | SELECT { core=
               SELECT_CORE { exprs; table=_; join=_; where=_; group_by=_; having=_ };
             order_by=_; limit=_; offset=_ } ->
    Expr.ty_expr_list exprs      
  | DELETE _ -> Nil
  | UPDATE _ -> Nil
  | INSERT _ -> Nil

let select exprs ~from:table_name =
  Types.SELECT_CORE {
    exprs; join=[]; table=table_name; where=None;
    group_by=None; having=None;
  }
let update ~table:table_name ~set =
  Types.UPDATE { table=table_name; on_err=None; where=None; set; }
let insert ~table:table_name ~values:set =
  Types.INSERT { table=table_name; on_err=None; set; }
let delete ~from:table_name =
  Types.DELETE { table=table_name; where=None }

let filter : ('a,'c) filter_fun
  = fun  by (type a b) (table : (b, a) t) : (b, a) t ->
    let update_where where by =
      match where with
        None -> Some by
      | Some old_by -> Some Expr.(by && old_by) in
    match table with
    | Types.SELECT_CORE { exprs; table; join; where; group_by; having } ->
      let where = update_where where by in
      SELECT_CORE { exprs; table; join; where; group_by; having }
    | Types.SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having }; order_by; limit; offset } ->
      let where = update_where where by in
      SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having }; order_by; limit; offset }
    | Types.DELETE { table; where } ->
      let where = update_where where by in
      DELETE { table; where }
    | Types.UPDATE { table; on_err; set; where } ->
      let where = update_where where by in
      UPDATE { table; on_err; set; where } 
    | Types.INSERT _ -> invalid_arg "filter on insert clause not supported"

let group_by : ('a,'b,'c) group_by_fun =
  fun by (type a b) (table : (b, a) t) : (b, a) t ->
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by=_; having } ->
    SELECT_CORE { exprs; table; join; where; group_by=Some by; having }
  | Types.SELECT { core=SELECT_CORE { exprs; table; join; where; group_by=_; having }; order_by; limit; offset } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by=Some by; having }; order_by; limit; offset }
  | Types.DELETE _ 
  | Types.UPDATE _ 
  | Types.INSERT _ -> invalid_arg "group by only supported on select clause"

let having : ('a,'c) having_fun =
  fun having (type a b) (table : (b, a) t) : (b, a) t ->
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by; having=_ } ->
    SELECT_CORE { exprs; table; join; where; group_by; having=Some having }
  | Types.SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having=_ }; order_by; limit; offset } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having=Some having }; order_by; limit; offset }
  | Types.DELETE _ 
  | Types.UPDATE _ 
  | Types.INSERT _ -> invalid_arg "group by only supported on select clause"

let join : ('a,'b,'d,'c) join_fun =
  fun ?(op=INNER) ~on (type a b c) (ot: (b, _) t)
    (table : (c, a) t)  ->
    match table with
    | Types.SELECT_CORE { exprs; table; join; where; group_by; having } ->
      Types.SELECT_CORE {
        exprs; table;
        join=join @ [MkJoin {
          table=ot;
          on;
          join_op=op
        }];
        where; group_by; having
      }
    | Types.SELECT _
    | Types.DELETE _ 
    | Types.UPDATE _ 
    | Types.INSERT _ ->
      invalid_arg "group by only supported on select clause"

let on_err : 'a . [`ABORT | `FAIL | `IGNORE | `REPLACE | `ROLLBACK ] -> ('c, 'a) t -> ('c, 'a) t =
  fun on_err (type a) (table : (_, a) t) : (_, a) t ->
  match table with
  | Types.SELECT_CORE _
  | Types.SELECT _
  | Types.DELETE _ -> invalid_arg "on_err only supported for update and insert"
  | Types.UPDATE { table; on_err=_; set; where } -> UPDATE { table; on_err=Some on_err; set; where }
  | Types.INSERT { table; on_err=_; set } -> INSERT { table; on_err=Some on_err; set } 

let limit by (table: (_, [< `SELECT | `SELECT_CORE] as 'ty) t) : (_, [> `SELECT]) t =
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by; having } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having }; limit=Some by; offset=None; order_by=None}
  | Types.SELECT { core; order_by; limit=_; offset } ->
    SELECT { core; order_by; limit=Some by; offset }

let offset by (table: (_, [< `SELECT | `SELECT_CORE] as 'ty) t) : (_, [> `SELECT]) t =
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by; having } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having }; limit=None; offset=Some by; order_by=None}
  | Types.SELECT { core; order_by; limit; offset=_ } ->
    SELECT { core; order_by; limit; offset=Some by }

let order_by ?(direction=`ASC) field (table: (_, [< `SELECT | `SELECT_CORE] as 'ty) t) : (_, [> `SELECT]) t =
  match table with
  | Types.SELECT_CORE { exprs; table; join; where; group_by; having } ->
    SELECT { core=SELECT_CORE { exprs; table; join; where; group_by; having }; limit=None; offset=None; order_by=Some (direction,field)}
  | Types.SELECT { core; order_by=_; limit; offset } ->
    SELECT { core; order_by= Some(direction,field); limit; offset }
