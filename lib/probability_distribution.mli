(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

type 'a t [@@deriving sexp]

module Elt : sig
  type 'a t

  val create : weight:float -> 'a -> 'a t

  val weight : 'a t -> float
end

(** These two fail if the sum of weights is zero or if any weight is
    negative. *)
val create_exn  : 'a   Elt.t list -> 'a t

(* CR-someday: this is unused. *)
val of_list_exn : 'a t Elt.t list -> 'a t

(* CR-someday: this is unused. *)
val singleton : 'a -> 'a t

val draw : 'a t -> 'a
