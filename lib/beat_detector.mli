(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common

type t

val create : unit -> t
val add : t -> time:Time.t -> value:float -> unit
val in_beat : t -> bool
val wave : t -> float

module Debug : sig
  val beat : t -> float option
  val beat_min : t -> float option
end
