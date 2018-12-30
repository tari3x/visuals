
module V = Vector.Float

type t

val var : int -> t
val const : float -> t

val ( * ) : t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t

val pow : t -> int -> t

val product : t list -> t
val sum : t list -> t

val scale : t -> float -> t

val zero_line_between_two_points : (float * float) -> (float * float) -> t

val to_maxima : t -> Maxima.Expr.t
val to_gnuplot : t -> string

module Datum : sig
  type t = V.t * float [@@deriving sexp]

  val weighted_average : t -> t -> w:float -> t
end

module Data : sig
  type t = Datum.t list
end

val error : t -> Data.t -> float

val lagrange
  :  degree:int
  -> Data.t
  -> t

