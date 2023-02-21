(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

(* CR-someday: make these use float, otherwise you get weird aliasing effects,
   like the darkening in fred when you set interpolation arg too low. *)

type t [@@deriving sexp]

val create : r:int -> g:int -> b:int -> a:float -> t
val components : t -> int * int * int * float
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
val none : t
val of_hex8_string : string -> t
val random : unit -> t
val random_interesting : unit -> t
val interpolate_two : t -> t -> float -> t

(* Defaults to white if list is empty *)
val interpolate : t list -> arg:float -> t

(* scale proportionally such that at least one coordinate will be max.  *)
val maximize : t -> t
val set_alpha : t -> alpha:float -> t
val scale : t -> by:float -> t

(*
val red : t -> int
val green : t -> int
val blue : t -> int
val alpha : t -> float
*)
