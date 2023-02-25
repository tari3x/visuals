(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Dom_wrappers
open Common

type t

(* CR-someday: [num_sources] only applies to beat sources. Make this more clear.  *)

(* To redirect the output of the sound card, choose "monitor of ..." in
   pavucontrol. *)
val create_from_mic : unit -> t Lwt.t
val create_from_html : id:string -> t
val start : t -> unit
val set_debug : t -> Ctx.t option -> unit

(* As in "source separation" *)
module Source : sig
  module Id : Id

  type t

  val id : t -> Id.t
end

module Listener : sig
  type t
end

module Event : sig
  (** Wave intensity normalized per second *)
  type t = Beat of Source.t | Delete of Source.t | Wave of float
end

(* CR-someday: this should just be a bus and multiplexing should be done by the
   user.
*)
val on_beat : t -> f:(Event.t -> unit) -> max_sources:int -> Listener.t
val on_wave : t -> f:(Event.t -> unit) -> Listener.t
val stop_listening : t -> Listener.t -> unit
