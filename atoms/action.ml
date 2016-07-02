open Common
open Geometry

module Kind = struct
  type t = [ `down | `up | `move ]
end

module Button = struct
  type t = [ `left | `right | `middle | `touch | `none ]
end

module Pointer_id = struct
  type t = string
  let create id =
    Printf.sprintf "pointer%d" id
end

module Pointer = struct
  type t =
    { id : Pointer_id.t
    ; position : Vector.t
    ; button : Button.t
    }

  let transform_position m t =
    { t with
      position = Matrix.apply m t.position
    }
end

module Pointer_action = struct
  type t =
    { kind : Kind.t
    ; changed_touches : Pointer.t list
    }

  let transform_positions m { kind; changed_touches } =
    { kind
    ; changed_touches = List.map changed_touches ~f:(Pointer.transform_position m)
    }
end

type t =
| Pointer of Pointer_action.t
| Set_color of Color_cycle.t

let transform_positions m = function
  | Set_color color -> Set_color color
  | Pointer p -> Pointer (Pointer_action.transform_positions m p)
