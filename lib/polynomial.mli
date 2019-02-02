open Float_array

module V = Vector.Float

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
val all_monomials : degree:int -> t list
val first_monomials : int -> t list

val to_string : t -> string
val to_maxima : t -> Maxima.Expr.t
val to_gnuplot : t -> string

val eval : t -> float list -> float
val eval_point : t -> V.t -> float

module Datum : sig
  type t = V.t * float [@@deriving sexp]

  val weighted_average : t -> t -> w:float -> t
end

module Data : sig
  type t = Datum.t list [@@deriving sexp]
end

val error : t -> Data.t -> float

module Basis : sig
  type t =
    { degree : int
    ; size : int
    }
end

val lagrange : degree:int -> Data.t -> t

(* The expert option if you only want to move a few points *)
module Lagrange : sig
  type poly = t
  type t

  val create
    :  basis:Basis.t
    -> Data.t
    -> t

  val add_data : t -> data:Data.t -> t

  val result : t -> poly
end

module Eval_ctx : sig
  type t

  val create : config:Config.t -> t

  val release : t -> unit
end

val values : Eval_ctx.t -> t -> A2.t
