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
val create_from_mic : max_sources:int -> t Lwt.t
val create_from_html : id:string -> max_sources:int -> t
val start : t -> unit
val set_debug : t -> Ctx.t option -> unit

(* As in "source separation" *)
module Source : sig
  module Id : Id

  type t

  val id : t -> Id.t
end

module Event : sig
  (** Wave intensity normalized per second *)
  type t =
    | Beat of Source.t
    | Delete of Source.t
    | Wave of float
end

(* CR-someday: make this a bus. Sadly it's tricky because of the finicky stuff
   we are doing inside.

   Hint: async_extra.async_bus
*)
val on_event : t -> f:(Event.t -> unit) -> unit
