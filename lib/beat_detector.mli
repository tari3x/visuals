(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common

type t

module Signal : sig
  type t =
    | Beat
    | Wave of float
end

val create : unit -> t
val add : t -> time:Time.t -> value:float -> unit
val signal : t -> Signal.t option

(* CR-soon: remove *)
val in_beat : t -> bool
val wave : t -> float option

module Debug : sig
  val long : t -> float option
  val wave : t -> float option
  val beat : t -> float option
  val beat_min : t -> float option
end
