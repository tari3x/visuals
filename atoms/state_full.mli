(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Dom_wrappers

(* CR: no support for Bezier in this one. *)

type t

val create : Ctx.t -> is_server:bool -> t Lwt.t

val process_action : t -> Action.t -> unit

val set_color : t -> Color_cycle.t -> unit

val on_box_active : t -> f:(Shape.t Box.t -> unit) -> unit

val most_recent : t -> Shape.t Box.t option

val set_shape : t -> Shape.t -> unit

val toggle_transient_mode : t -> unit
