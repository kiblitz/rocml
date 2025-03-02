open! Core
open! Import

module%gen rec Expression : sig
  type t =
    | Identifier of Identifier.t
    | Constant of Constant.t
    | Assign of
        { assignments : Assignment.t list
        ; within_scope : Expression.t
        }
    | Apply of
        (* TODO infix *)
        
        { f : t
        ; args : Arg.t list
        }
    | If_then_else of
        { condition : Expression.t
        ; then_ : Expression.t
        ; else_ : Expression.t option
        }
    | For of
        { ident : Identifier.t
        ; from : Expression.t
        ; to_ : Expression.t
        ; body : Expression.t
        ; direction : Direction.t
        }
    | While of
        { condition : Expression.t
        ; body : Expression.t
        }
    | Sequence of
        { this : Expression.t
        ; next : Expression.t
        }
  [@@deriving sexp_of]
end =
  Expression

and Assignment : sig
  type t =
    { ident : Identifier.t
    ; value : Expression.t
    }
  [@@deriving sexp_of]
end =
  Assignment

and Arg : sig
  module Label : sig
    type t =
      | Unnamed
      | Labelled of string
      | Optional of string
    [@@deriving sexp_of]
  end

  type t =
    { label : Label.t
    ; expression : Expression.t
    }
  [@@deriving sexp_of]
end =
  Arg

and Identifier : sig
  type t = { name : string } [@@deriving sexp_of]
end =
  Identifier

and Constant : sig
  type t =
    | Bool of bool
    | Int of int
    | Float of float
  [@@deriving sexp_of]
end =
  Constant

and Direction : sig
  type t =
    | Up
    | Down
  [@@deriving sexp_of]
end =
  Direction

type t =
  { func_args : Identifier.t list
  ; body : Expression.t
  }
[@@deriving sexp_of]
