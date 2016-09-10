open Geometry
open Dom_wrappers

val default_line_width : float

module Kind : sig
  (* CR: change [Gui_full] to hide this definition. *)
  type t =
  | Rectangle
  | Circle
  | Horizontal_line
  | Vertical_line
  | Cross_line
  | Zigzag of Vector.t list
  | Bezier of Vector.t list

  val examples : t list
end

type t

val default : t

val to_string : t -> string

val create
  :  kind:Kind.t
  -> frame:Frame.t
  -> color:Color_cycle.t
  -> ?line_width:float
  -> unit
  -> t

val frame : t -> Frame.t
val color : t -> Color_cycle.t

val set
  :  ?kind:Kind.t
  -> ?frame:Frame.t
  -> ?color:Color_cycle.t
  -> ?line_width:float
  -> t
  -> t

val set_touches : t -> touches:Vector.t list -> t
val set_translation : t -> Vector.t -> t
val set_alpha : t -> alpha:float -> t

val render
  :  t
  -> Ctx.t
  -> time:float
  -> unit

val touched_by : t -> Vector.t -> bool

(* Assuming no scaling is already applied. *)
val scale_to_fit : t -> float -> t

(* Shape together with the viewport scale in which it was created. *)
module Local : sig
  type t
  val to_string : t -> string
end

val to_local : t -> viewport_scale:float -> Local.t
val of_local : Local.t -> viewport_scale:float -> t
