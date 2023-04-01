open Geometry

module Matrix : sig
  type t

  val create : Matrix.t -> t
end

type t

val init_exn : unit -> t
val clear : t -> unit
val width : t -> float
val height : t -> float
val draw_circle : t -> x:float -> y:float -> radius:float -> unit
val begin_fill : t -> Color.t -> unit
val end_fill : t -> unit
val move_to : t -> Vector.t -> unit
val line_to : t -> Vector.t -> unit
val path : t -> closed:bool -> Vector.t list -> unit
val close_path : t -> unit

val line_style
  :  t
  -> ?width:float
  -> ?color:
       Color.t
       (*
  -> ?alignment:float
  -> ?native:bool Js.t
  *)
  -> unit
  -> unit

val actions : t -> Action.t Lwt_stream.t
val set_matrix : t -> Matrix.t -> unit
