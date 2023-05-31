module Schema = Schema
module Query = Query
module Expr = Expr
module Types = Types

module Type = struct
  include Type

  let pp fmt vl = Format.fprintf fmt "%s" (show vl)
end

type table_name = Types.table_name
type ('ret_ty, 'query_kind) query = ('ret_ty, 'query_kind) Types.query
type ('res, 'multiplicity) request = ('res, 'multiplicity) Request.t

module Request = Request

module Sqlite3 = struct
  module Request = Request

  module Expr = struct
    type 'a t = 'a Expr.t

    type 'a expr_list = 'a Expr.expr_list =
      | [] : unit expr_list
      | ( :: ) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

    type wrapped_assign = Types.wrapped_assign

    include Expr.Sqlite3
  end

  module Type = struct
    type 'a t = 'a Type.t

    let int = Type.int
    let real = Type.real
    let text = Type.text
    let bool = Type.bool

    include Type.Sqlite3
    module Numeric = Type.Numeric
  end
end

module Postgres = struct
  module Request = Request

  module Expr = struct
    type 'a t = 'a Expr.t

    type 'a expr_list = 'a Expr.expr_list =
      | [] : unit expr_list
      | ( :: ) : ('a t * 'b expr_list) -> ('a * 'b) expr_list

    type wrapped_assign = Types.wrapped_assign

    include Expr.Postgres
  end

  module Type = struct
    type 'a t = 'a Type.t

    let int = Type.int
    let real = Type.real
    let text = Type.text
    let bool = Type.bool

    include Type.Postgres
    module Numeric = Type.Numeric
  end
end

let result_all_unit : (unit, 'e) result list -> (unit, 'e) result =
 fun ls ->
  let rec loop = function
    | [] -> Ok ()
    | Ok () :: t -> loop t
    | Error err :: _ -> Error err
  in
  loop ls

let rec drop_while ~f ls =
  match ls with [] -> [] | h :: t when f h -> drop_while ~f t | ls -> ls
