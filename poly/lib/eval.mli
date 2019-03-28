module P = Polynomial

open Float_array

module Ctx : sig
  type t

  val create : config:Config.t -> degree:int -> t

  val release : t -> unit
end

val values : Ctx.t -> P.t -> A2_int.t
