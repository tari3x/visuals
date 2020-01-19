(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

type t

val create : window:float -> t
val add : t -> param:float -> value:float -> unit
val get : t -> float option
val get_exn : t -> float
