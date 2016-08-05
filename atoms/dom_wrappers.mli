open Common
open Geometry

module Ctx : sig
  type t

  val create : id:string -> width:int -> height:int -> t

  val canvas_actions : t -> Action.t Lwt_stream.t

  val clear : t -> unit

  val width : t -> float
  val height : t -> float

  val draw_circle : t -> Vector.t -> radius:float -> unit

  val draw_horizontal_line : t -> Vector.t -> width:float -> unit
  val draw_vertical_line   : t -> Vector.t -> width:float -> unit

  val fill_rect : t -> Vector.t -> width:float -> height:float -> unit

  val draw_centered_rectangle : t -> width:float -> height:float -> unit

  val set_fill_color : t -> Color.t -> unit
  val set_stroke_color : t -> Color.t -> unit

  val clip_rect : t -> Vector.t -> width:int -> height:int -> unit

  val set_font : t -> string -> unit
  val fill_text : t -> string -> float -> float -> unit
  val stroke_text : t -> string -> float -> float -> unit

  val save : t -> unit
  val restore : t -> unit

  val transform : t -> Matrix.t -> unit
end

