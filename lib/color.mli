(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)


type t [@@deriving sexp]

val create : r:int -> g:int -> b:int -> a:float -> t

val r : t -> int
val g : t -> int
val b : t -> int
val a : t -> float

val to_string : t -> string

val white : t
val red : t
val green : t
val blue : t
val cyan : t
val magenta : t
val yellow : t
val dark_orange : t
val black : t

val of_hex8_string : string -> t

val random : unit -> t

(* Defaults to white if list is empty *)
val interpolate : t list -> arg:float -> t

(* scale proportionally such that at least one coordinate will be max.  *)
val maximize : t -> t

val set_alpha : t -> alpha:float -> t
val scale : t -> float -> t

(*
val red : t -> int
val green : t -> int
val blue : t -> int
val alpha : t -> float
*)
