
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

(* 2D *)
val all_monomials : degree:int -> t list

val to_string : t -> string
val to_maxima : t -> Maxima.Expr.t
val to_gnuplot : t -> string

val eval : t -> float list -> float

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

module Grid : sig
  open Bigarray
  type t = (float, float64_elt, c_layout) Array2.t
end

module Mono_cache : sig
  type t

  (* CR-someday: could be adding elements on demand. *)
  val create : config:Config.t -> degree:int -> t
end

val eval_on_grid : t -> cache:Mono_cache.t -> Grid.t
