open Common
open Action
open Geometry

type t

module Update : sig
  module Single : sig
    type t
    val apply : t -> Shape.t -> Shape.t
  end

  (* [None] means the shape is not touched any more. *)
  type t = (Shape_id.t * Single.t option) list
end

val create : unit -> t

val is_touching : t -> Shape_id.t -> bool

val add : t -> Shape_id.t -> Frame.t -> Pointer.t -> Update.t

val move : t -> Pointer.t list -> Update.t

val remove : t -> Pointer.t list -> Update.t

val active : t -> Shape_id.t list
