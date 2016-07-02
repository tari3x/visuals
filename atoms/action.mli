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
end

module Pointer_action : sig
  type t =
    { kind : Kind.t
    ; changed_touches : Pointer.t list
    }
end

type t =
| Pointer of Pointer_action.t
| Set_color of Color_cycle.t

val transform_positions : Matrix.t -> t -> t

