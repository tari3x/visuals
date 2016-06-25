open Geometry
open Dom_wrappers

type t

val dummy : t

val to_string : t -> string

val create : Vector.t -> Color_cycle.t -> t

val render : t -> Ctx.t -> time:float -> unit

val frame : t -> Frame.t
val set_frame : t -> Frame.t -> t

val color     : t -> Color_cycle.t
val set_color : t -> Color_cycle.t -> t

val touched_by : t -> Vector.t -> bool

