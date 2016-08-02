open Geometry

module Kind : sig
  type t = [ `down | `up | `move ]
end

module Button : sig
  type t = [ `left | `right | `middle | `touch | `none ]
end

module Pointer_id : sig
  type t
  val create : int -> t
end

(* Either a touch or a mouse. *)
module Pointer : sig
  type t =
    { id : Pointer_id.t
    ; position : Vector.t
    ; button : Button.t
    }

  val id : t -> Pointer_id.t
end

type t =
  { kind : Kind.t
  ; changed_touches : Pointer.t list
  }

val to_string : t -> string

val coords : t -> Vector.t
