open Async

module P = Polynomial

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
    ; show_dots : bool
    }
end

module State : sig
  type t

  val create : P.t -> t

  val collapse : t -> t

  val emerge : t -> P.t -> t Deferred.t
end

val write
  :  dir:string
  -> config:Config.t
  -> State.t list
  -> unit Deferred.t
