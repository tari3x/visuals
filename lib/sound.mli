(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Dom_wrappers
open Common

type t

(* To redirect the output of the sound card, choose "monitor of ..." in
   pavucontrol. *)
val create_from_mic : unit -> t Lwt.t

val create_from_html : id:string -> t

val set_debug : t -> Ctx.t option -> unit

(* As in "source separation" *)
module Source : sig
  module Id : Id
  type t
  val id : t -> Id.t
  val color : t -> Color.t
end

val on_beat : t -> f:(Source.t -> unit) -> unit
