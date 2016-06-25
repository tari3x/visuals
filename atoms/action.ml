open Common
open Geometry

module Kind = struct
  type t = [ `down | `up | `move ]
end

module Button = struct
  type t = [ `left | `right | `middle | `touch | `none ]
end

module Pointer_id = struct
  module T = struct
    type t = int
    let to_string t =
      Printf.sprintf "pointer%d" t
  end
  include  T
  module Table = Make_table(T)
end

module Pointer = struct
  type t =
    { id : Pointer_id.t
    ; position : Vector.t
    ; button : Button.t
    }
end

module Pointer_action = struct
  type t =
    { kind : Kind.t
    ; changed_touches : Pointer.t list
    }
end

type t =
| Pointer of Pointer_action.t
| Set_color of Color_cycle.t
