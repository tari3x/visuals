(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

type t

val of_hertz : float -> t

val hertz : t -> float
val mel : t -> float
