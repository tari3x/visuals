open Dom_wrappers

(* CR: no support for Bezier in this one. *)

type t

val create : Ctx.t -> is_server:bool -> t Lwt.t

val process_action : t -> Action.t -> unit

val set_color : t -> Color_cycle.t -> unit

val on_shape_active : t -> f:(Shape.t -> unit) -> unit

val most_recent : t -> Shape.t option

val set_shape_kind : t -> Shape.Kind.t -> unit

val toggle_transient_mode : t -> unit
