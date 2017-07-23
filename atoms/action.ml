(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common
open Geometry
open Printf

module Kind = struct
  type t = [ `down | `up | `move ]
  [@@deriving compare]

  let to_string = function
    | `down -> "down"
    | `up   -> "up"
    | `move -> "move"

  let equal =
    [%compare.equal: t]
end

module Button = struct
  type t = [ `left | `right | `middle | `touch | `none ]

  let to_string = function
    | `left -> "left"
    | `right -> "right"
    | `middle -> "middle"
    | `touch -> "touch"
    | `none -> "none"
end

module Pointer_id = struct
  include String

  let create id =
    Printf.sprintf "pointer%d" id
end

module Pointer = struct
  type t =
    { id : Pointer_id.t
    ; position : Vector.t
    }

  let id t =
    t.id

  let to_string t =
    Vector.to_string t.position
end

type t =
  { kind : Kind.t
  ; changed_touches : Pointer.t list
  ; button : Button.t
  }

let to_string { kind; changed_touches; button = _ } =
  sprintf "{ kind = %s; changed_touches = (%s)}"
    (Kind.to_string kind)
    (String.concat ~sep:" " (List.map changed_touches ~f:Pointer.to_string))

let coords t =
  let p = List.hd_exn t.changed_touches in
  p.position

let click t =
  Option.some_if (Kind.equal t.kind `down) (coords t)

