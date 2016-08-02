open Geometry
open Dom_wrappers

module Kind : sig
  type t =
  | Rectangle
  | Circle
  | Horizontal_line
  | Vertical_line
  | Cross_line
  | Bezier of Vector.t list

  val examples : t list
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
  -> unit

val frame : t -> Frame.t
val add_frame : t -> frame:Frame.t -> t
val set_frame : t -> frame:Frame.t -> t

val set_touches : t -> Vector.t list -> t

val color     : t -> Color_cycle.t

val touched_by : t -> Vector.t -> bool

val set_translation : t -> Vector.t -> t

val set_alpha : t -> alpha:float -> t

(* Assuming no scaling is already applied. *)
val scale_to_fit : t -> float -> t

(* Shape together with the viewport scale in which it was created. *)
module Local : sig
  type t
  val to_string : t -> string
end

val to_local : t -> viewport_scale:float -> Local.t
val of_local : Local.t -> viewport_scale:float -> t
