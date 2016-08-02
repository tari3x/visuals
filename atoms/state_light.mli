open Dom_wrappers

type t

val create : Ctx.t -> Shape.t -> t Lwt.t

val process_action : t -> Action.t -> unit

val set_color : t -> Color_cycle.t -> unit
