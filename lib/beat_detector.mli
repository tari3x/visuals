(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common

type t

val create : unit -> t

val add_sample : t -> time:Time.t -> value:float -> unit

val in_beat : t -> bool

module Debug : sig
  val ewma : t -> float option
end
