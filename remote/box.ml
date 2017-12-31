(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

let default_line_width = 40.

type coordinates = [ `internal | `canvas ]

type 'a t =
  { kind : 'a
  ; frame : Frame.t
  ; color_cycle : Color_cycle.t
  ; line_width : float
  ; touches : Vector.t list
  } [@@deriving fields, sexp]

let color t =
  Color_cycle.nth_defaulting_to_last_or_white t.color_cycle 0

let create ~kind
    ?(frame = Frame.ident)
    ?(color_cycle = Color_cycle.default)
    ?(line_width = default_line_width)
    () =
  { kind; frame; color_cycle; line_width; touches = [] }

let set t
    ?(kind = t.kind)
    ?(frame = t.frame)
    ?(color_cycle = t.color_cycle)
    ?(line_width = t.line_width)
    () =
  let touches = t.touches in
  { kind; frame; color_cycle; line_width; touches }

let set ?kind ?frame ?color_cycle ?line_width t =
  set ?kind ?frame ?color_cycle ?line_width t ()

let set_touches t ~coordinates ~touches =
  let touches =
    match coordinates with
    | `internal -> touches
    | `canvas   ->
      let m = Frame.matrix t.frame |> Matrix.inv in
      List.map touches ~f:(Matrix.apply m)
  in
  { t with touches }

let touches t ~coordinates =
  match coordinates with
  | `internal -> t.touches
  | `canvas   ->
    let m = Frame.matrix t.frame in
    List.map t.touches ~f:(Matrix.apply m)

(*
  let to_string t =
  let { kind; frame; color_cycle; line_width } = t in
  Printf.sprintf "{ %s; %s; %s; %f }"
  (Kind.to_string kind)
  (Frame.to_string frame)
  (Color_cycle.to_string color_cycle)
  line_width

*)

let default kind =
  create ~kind ()

let set_alpha t ~alpha =
  set t ~color_cycle:(Color_cycle.set_alpha (color_cycle t) ~alpha)

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
