open Async

module V = Vector.Float

type t

val var : int -> t
val func : string -> t list -> t
val const : float -> t

val ( * ) : t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t

val pow : t -> int -> t

val product : t list -> t
val sum : t list -> t

val scale : t -> float -> t

(* quotient and remainder *)
module Division_result : sig
  type nonrec t = { q : t; r : t }
end

val divide : t -> t -> Division_result.t Deferred.t

val zero_line_between_two_points : (float * float) -> (float * float) -> t

val to_gnuplot : t -> string

module Datum : sig
  type t = V.t * float [@@deriving sexp]

  val weighted_average : t -> t -> w:float -> t
end

val lagrange : degree:int -> Datum.t list -> t Deferred.t
