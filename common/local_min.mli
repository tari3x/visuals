(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

type t

(** Starts with 0. *)
val create : unit -> t

val add : t -> float -> unit
val get : t -> float
