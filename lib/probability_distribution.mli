(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

type 'a t [@@deriving sexp]

(* Fails if the sum of weights is zero or if any weight is negative. *)
val create_exn : ('a * float) list -> 'a t

val draw : 'a t -> 'a
