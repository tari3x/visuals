open Geometry

module Matrix : sig
  type t

  val create : Matrix.t -> t
end

module Graphics : sig
  include module type of Pixi_lib.Graphics

  val draw_circle : t -> x:float -> y:float -> radius:float -> unit

  val draw_rect
    :  t
    -> x:float
    -> y:float
    -> width:float
    -> height:float
    -> unit

  val fill : t -> Color.t -> unit
  val stroke : t -> Color.t -> width:float -> unit
  val move_to : t -> Vector.t -> unit
  val line_to : t -> Vector.t -> unit
  val path : t -> closed:bool -> Vector.t list -> unit
  val close_path : t -> unit
  val set_tint_and_alpha : t -> Color.t -> unit
  val set_visible : t -> bool -> unit
  val set_transform : t -> Matrix.t -> unit
  val set_zindex : t -> int -> unit
end

type t

val init_exn : unit -> t Lwt.t
val create_graphics : t -> Graphics.t
val width : t -> float
val height : t -> float
val actions : t -> Action.t Lwt_stream.t
