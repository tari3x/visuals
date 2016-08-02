open Common
open Geometry

module Kind = struct
  type t = [ `down | `up | `move ]

  let to_string = function
    | `down -> "down"
    | `up   -> "up"
    | `move -> "move"
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

  let id t =
    t.id
end

type t =
  { kind : Kind.t
  ; changed_touches : Pointer.t list
  }

let to_string t =
  Kind.to_string t.kind

let coords t =
  let p = List.hd t.changed_touches in
  p.position
