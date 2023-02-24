(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

type 'a t [@@deriving sexp]

module Elt : sig
  type 'a t

  (* Fails if the weight is negative. *)
  val create_exn : weight:float -> 'a -> 'a t option
  val weight : 'a t -> float
end

(** Fails if the sum of weights is zero. *)
val create_exn : 'a Elt.t list -> 'a t

val draw : 'a t -> 'a
