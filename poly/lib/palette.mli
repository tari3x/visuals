open Async

(* CR-someday: use [Visuals.Color]? Why would I need alpha? How does one add
   alphas? *)

module Color : sig
  type t = Color.rgb [@@deriving sexp]

  val graphics_color : t -> Graphics.color

  val black : t
  val red : t
end

  (* A triplet of colours that add up to white. *)
module Basis : sig
  type t [@@deriving sexp]

  val rgb : t
  val random : unit -> t

  val pipe : Config.t -> t Pipe.Reader.t
end

type t = Color.t array

val num_colors : int
val create : Basis.t -> Color.t array
val create_graphics : Basis.t -> Graphics.color array

val rgb : Color.t array
val rgb_graphics : Graphics.color array
