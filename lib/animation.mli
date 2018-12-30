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
    ; cbrange : int * int
    ; show_dots : V.t list
    }
end

module State : sig
  type t

  val of_poly
    :  ?show_dots:V.t list
    -> P.t
    -> t

  val of_maxima
    :  ?show_dots:V.t list
    -> E.t
    -> t

  val collapse : t -> t

  val emerge : t -> P.t -> t Deferred.t
end

val write
  :  dir:string
  -> config:Config.t
  -> ?interpolate:bool
  -> State.t list
  -> unit Deferred.t
