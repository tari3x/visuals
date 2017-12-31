(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Dom_wrappers

type t

val create_from_mic : unit -> t Lwt.t

val create_from_html : id:string -> t

val on_beat : t -> f:(unit -> unit) -> unit

val set_debug : t -> Ctx.t option -> unit
