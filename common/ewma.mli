(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

type t

val create : half_life:float -> t

val add_sample : t -> param:float -> value:float -> unit

val value : t -> float option
val value_exn : t -> float
