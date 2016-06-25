open Common
open Geometry

module Mouse_event : sig
  type t = Dom_html.mouseEvent Js.t

  val action : t -> Action.Kind.t -> Action.t
end

(* CR: make it work with multi-touch. *)
module Touch_event : sig
  type t = Dom_html.touchEvent Js.t

  val action : t -> Action.Kind.t -> Action.t
end

module Ctx : sig
  type t = Html.canvasRenderingContext2D Js.t

  val clear : t -> unit

  val draw_circle : t -> Vector.t -> radius:float -> unit

  val draw_horizontal_line : t -> Vector.t -> width:float -> unit
  val draw_vertical_line   : t -> Vector.t -> width:float -> unit

  val draw_centered_rectangle : t -> width:float -> height:float -> unit

  val set_fill_color : t -> Color.t -> unit

  val save : t -> unit
  val restore : t -> unit

  val transform : t -> Matrix.t -> unit
end

