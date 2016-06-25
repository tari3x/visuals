open Common
open Action
open Geometry

type t

val create : unit -> t

val is_touching : t -> Shape_id.t -> bool

val add : t -> Shape_id.t -> Frame.t -> Pointer.t -> unit

val move : t -> Pointer.t list -> (Shape_id.t * Frame.t) list

(* Returns the list of shapes that are not touched any more. *)
val remove : t -> Pointer.t list -> Shape_id.t list
