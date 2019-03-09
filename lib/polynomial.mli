open Float_array

module V = Vector.Float

module Var : sig
  type t

  val create : int -> t
end

type t

val var : int -> t
(* 2D *)
val call : string -> t
val verbatim : string -> t
val const : float -> t

val ( * ) : t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t

val pow : t -> int -> t

val product : t list -> t
val sum : t list -> t

val scale : t -> float -> t

val zero_line_between_two_points : (float * float) -> (float * float) -> t

val monomials : t -> t list

(* 2D *)
val first_monomials : int -> t list

val to_string : t -> string
val to_maxima : t -> Maxima.Expr.t
val to_gnuplot : t -> string

val eval : t -> float list -> float
val eval_point : t -> V.t -> float

val subst : t -> var:Var.t -> by:t -> t

module Basis : sig
  module Kind : sig
    type t

    val mono : degree:int -> t
    val bernstein : degree:int -> domain:(V.t * V.t) -> t
  end

  val create : Kind.t -> t list
end

module Datum : sig
  type t = V.t * float [@@deriving sexp]

  val weighted_average : t -> t -> w:float -> t
end

module Data : sig
  type t = Datum.t list [@@deriving sexp]
end

val error : t -> Data.t -> float

val lagrange : basis:Basis.Kind.t -> Data.t -> t

(* The expert option if you only want to move a few points *)
module Lagrange : sig
  type poly = t
  type t

  val create
    :  basis:Basis.Kind.t
    -> size_unused:int
    -> Data.t
    -> t

  val add_data : t -> data:Data.t -> t

  val result : t -> poly
end

module Eval_ctx : sig
  type t

  val create : config:Config.t -> degree:int -> t

  val release : t -> unit
end

val values : Eval_ctx.t -> t -> A2.t
