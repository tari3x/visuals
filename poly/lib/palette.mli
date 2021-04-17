open Async

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

(* CR: this will not be necessary *)
val create_graphics : Basis.t -> Graphics.color array
val rgb : Color.t array
val rgb_graphics : Graphics.color array
