(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open! Common
open Geometry

let default_line_width = 40.

type 'a t =
  { kind : 'a
  ; frame : Frame.t
  ; color : Color_cycle.t
  ; line_width : float
  ; touches : Vector.t list
  } [@@deriving fields, sexp]

let create ~kind
    ?(frame = Frame.ident)
    ?(color = Color_cycle.default)
    ?(line_width = default_line_width)
    () =
  { kind; frame; color; line_width; touches = [] }

let set t
    ?(kind = t.kind)
    ?(frame = t.frame)
    ?(color = t.color)
    ?(line_width = t.line_width)
    () =
  let touches = t.touches in
  { kind; frame; color; line_width; touches }

let set ?kind ?frame ?color ?line_width t =
  set ?kind ?frame ?color ?line_width t ()

let set_touches t ~coordinates ~touches =
  let touches =
    match coordinates with
    | `internal -> touches
    | `canvas   ->
      let m = Frame.matrix t.frame |> Matrix.inv in
      List.map touches ~f:(Matrix.apply m)
  in
  { t with touches }

(*
  let to_string t =
  let { kind; frame; color; line_width } = t in
  Printf.sprintf "{ %s; %s; %s; %f }"
  (Kind.to_string kind)
  (Frame.to_string frame)
  (Color_cycle.to_string color)
  line_width

*)

let default kind =
  create ~kind ()

let set_alpha t ~alpha =
  set t ~color:(Color_cycle.set_alpha (color t) ~alpha)

let set_translation t v =
  set t ~frame:(Frame.set_translation (frame t) v)

module Local = struct
  type 'a box = 'a t [@@deriving sexp]
  type 'a t =
    { box : 'a box
    ; scale : float
    } [@@deriving sexp]

(*
  let to_string t =
    Printf.sprintf "{%s, scale = %f}"
      (to_string t.shape) t.scale
*)
end

let to_local t ~viewport_scale =
  { Local. box = t; scale = viewport_scale }

let of_local (local : 'a Local.t) ~viewport_scale =
  let scale = viewport_scale /. local.scale in
  let t = local.box in
  set t ~frame:(Frame.scale_viewport (frame t) scale)
