open Core

module V = Vector.Float

module Var : sig
  type t

  val create : int -> t
end

type t [@@deriving compare, sexp_of]

include Comparable.S with type t := t

val var : int -> t
val x : t
val y : t
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

val scale : t -> by:float -> t

val zero_line_between_two_points : (float * float) -> (float * float) -> t

val monomials : t -> (t * float) list

(* 2D *)
val first_monomials : int -> t list

val to_string : t -> string
val to_maxima : t -> Maxima.Expr.t
val to_gnuplot : t -> string

val eval : t -> float list -> float
val eval_point : t -> V.t -> float

val subst : t -> var:Var.t -> by:t -> t

val distance : t -> t -> float

module Basis : sig
  val mono : degree:int -> t list
  val bernstein : degree:int -> domain:(V.t * V.t) -> t list
end
