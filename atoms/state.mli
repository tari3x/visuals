open Dom_wrappers

type t

val create : Ctx.t -> is_leader:bool -> t

val process_action : t -> Action.t -> unit

val on_shape_active : t -> f:(Shape.t -> unit) -> unit

val last_touched : t -> Shape.t option
