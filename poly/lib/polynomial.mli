open Core
open Async
module V = Vector.Float

(** Variables to be called [v_x, v_y], etc. Values to be called something
    else. [Var.x] is ok.
*)
module Var : sig
  type t

  val x : t
  val y : t
  val create : int -> t
end

type t [@@deriving compare, sexp]

include Comparable.S with type t := t

val var : Var.t -> t
val x : t
val y : t
val const : float -> t
val ( * ) : t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val pow : t -> int -> t
val product : t list -> t
val sum : t list -> t
val scale : t -> by:float -> t
val zero_line_between_two_points : V.t -> V.t -> t
val monomials : t -> (t * float) list
val first_monomials : int -> t list
val to_string : t -> string
val eval : t -> float list -> float
val subst : t -> var:Var.t -> by:t -> t
val distance : t -> t -> float
val weighted_average : t -> t -> w:float -> t

(** 2D *)
val eval_point : t -> V.t -> float

(** 2D *)
module Basis : sig
  type nonrec t = t list

  val mono : degree:int -> t
  val bernstein : degree:int -> domain:V.t * V.t -> t
  val odd_powers_only : t -> t
end

(** Quotient and remainder *)
module Division_result : sig
  type nonrec t =
    { q : t
    ; r : t
    }
end

val divide : t -> t -> Division_result.t Deferred.t
val is_roughly_zero : t -> bool
