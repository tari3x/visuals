open Geometry
open Dom_wrappers

module Kind : sig
  type t =
  | Rect
  | Circle
  | Horizontal_line
  | Vertical_line
  | Cross_line
end

type t =
  { kind : Kind.t
  ; frame : Frame.t
  ; color : Color_cycle.t
  }

val default : t

val to_string : t -> string

val create
  :  kind:Kind.t
  -> frame:Frame.t
  -> color:Color_cycle.t
  -> t

val render
  :  t
  -> Ctx.t
  -> time:float
  -> transform:Matrix.t
  -> unit

val frame : t -> Frame.t
val add_frame : t -> Frame.t -> t
val set_frame : t -> Frame.t -> t

val color     : t -> Color_cycle.t

val touched_by : t -> Vector.t -> bool

val set_translation : t -> Vector.t -> t

val set_alpha : t -> alpha:float -> t
