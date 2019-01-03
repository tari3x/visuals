open Async

module P = Polynomial
module E = Maxima.Expr
module V = Vector.Float

module Config : sig
  type t =
    { n_x : int
    ; n_y : int
    ; left_margin : int
    ; top_margin : int
    ; right_margin : int
    ; bottom_margin : int
    ; style : [ `zeroes | `heat ]
    ; cbrange : float * float
    ; show_dots : V.t list
    }
end

module State : sig
 type t =
   { p : P.t
   ; ps : P.t list
   ; defs : (string * P.t) list
   ; dots : V.t list
   }

  val of_poly
    :  ?show_dots:V.t list
    -> P.t
    -> t

  val collapse : t -> t

  val emerge : t -> P.t -> t Deferred.t

  val interpolate : t list -> t list
end

type t =
  { config : Config.t
  ; states : State.t list
  }

val create : config:Config.t -> State.t list -> t
