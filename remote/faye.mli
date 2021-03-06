(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base

module Channel : sig
  type t [@@deriving sexp]

  val global : string -> t
  val create : unit -> t
end

type 'a t

val create_exn : sexp_of_a:('a -> Sexp.t) -> 'a t
val publish : 'a t -> Channel.t -> 'a -> unit

(* Clients will receive their own messages. *)
val subscribe_with_try : 'a t -> Channel.t -> f:('a -> unit) -> unit
