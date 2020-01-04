(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

(** Sliding window min *)

type t

val create : window:float -> t
val add : t -> param:float -> value:float -> unit
val min : t -> float option
