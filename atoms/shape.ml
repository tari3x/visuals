(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Geometry
open Dom_wrappers

(* CR: lines and cross lines get scaled down *)

let rect_width  = 200.
let rect_height = 200.
let circle_radius = 100.
let line_touch_width = 60.

type t =
| Rectangle
| Circle
| Horizontal_line
| Vertical_line
| Cross_line
| Zigzag
| Bezier
[@@deriving sexp]

let default = Rectangle

let render_zigzag ctx vs =
  let rec loop = function
    | [] -> ()
    | v :: vs ->
      Ctx.line_to ctx v;
      loop vs
  in
  match vs with
  | [] -> ()
  | v :: vs ->
    Ctx.save ctx;
    Ctx.begin_path ctx;
    Ctx.move_to ctx v;
    loop vs;
    Ctx.stroke_without_transform ctx;
    Ctx.restore ctx

let render_bezier ctx vs =
  let make_cp v1 v2 v3 =
    let v = Vector.((v3 - v1) / 2.) in
    Vector.[ v2 - v; v2 + v ]
  in
  let rec control_points = function
    | [] | [ _ ] -> []
    | [ _; v2 ] -> [ v2 ]
    | v1 :: v2 :: v3 :: vs ->
      make_cp v1 v2 v3 @ control_points (v2 :: v3 :: vs)
  in
  let rec loop vs ~control:cs =
    match vs, cs with
    | [], [] -> ()
    | v :: vs, c1 :: c2 :: cs ->
      Ctx.bezier_curve_to ctx ~control1:c1 ~control2:c2 v;
      loop vs ~control:cs
    | _ ->
      failwith "BUG in render_bezier"
  in
  match vs with
  | [] | [ _ ] | [ _; _ ] -> render_zigzag ctx vs
  | v :: vs ->
    let control = v :: control_points (v :: vs) in
    Ctx.save ctx;
    Ctx.begin_path ctx;
    Ctx.move_to ctx v;
    loop vs ~control;
    Ctx.stroke_without_transform ctx;
    Ctx.restore ctx

let rec render t ctx ~touches =
  match t with
  | Rectangle ->
    Ctx.draw_centered_rectangle ctx ~width:rect_width ~height:rect_height
  | Circle ->
    Ctx.draw_circle ctx Vector.zero ~radius:circle_radius
  | Horizontal_line ->
    Ctx.draw_horizontal_line ctx Vector.zero
  | Vertical_line ->
    Ctx.draw_vertical_line ctx Vector.zero
  | Cross_line ->
    render Horizontal_line ctx ~touches;
    render Vertical_line ctx ~touches
  | Zigzag -> render_zigzag ctx touches
  | Bezier -> render_bezier ctx touches

let render box ctx ~time =
  let { Box.
        kind = t
      ; frame
      ; color
      ; line_width
      ; touches
      } = box
  in
  let frame =
    match t with
    | Circle ->
      Frame.equal_scale frame
    | Horizontal_line
    | Vertical_line
    | Cross_line ->
      Frame.remove_scale frame
    | Zigzag
    | Bezier
    | Rectangle -> frame
  in
  Ctx.save ctx;
  let current_color = Color_cycle.current_color color ~time in
  Ctx.set_fill_color ctx current_color;
  Ctx.set_stroke_color ctx current_color;
  Ctx.set_line_width ctx line_width;
  Ctx.set_line_join ctx `round;
  Ctx.set_line_cap ctx `round;
  Ctx.transform ctx (Frame.matrix frame);
  render t ctx ~touches;
  Ctx.restore ctx

let rec touched_by t p =
  let open Float in
  let (x, y) = Vector.coords p in
  match t with
  | Rectangle ->
    abs x <= (rect_width /. 2.) && abs y <= (rect_height /. 2.)
  | Circle ->
    Vector.length p <= circle_radius
  | Horizontal_line ->
    abs x <= line_touch_width
  | Vertical_line ->
    abs y <= line_touch_width
  | Cross_line ->
    touched_by Horizontal_line p || touched_by Vertical_line p
  | Zigzag -> false
  | Bezier -> false

let touched_by box p =
  let { Box.
        kind = t
      ; frame
      ; color = _
      ; line_width = _
      ; touches = _
      } = box
  in
  (* CR-someday: should this be like [render]? *)
  let frame =
    match t with
    | Horizontal_line
    | Vertical_line
    | Cross_line -> Frame.remove_scale frame
    | Circle
    | Rectangle
    | Zigzag
    | Bezier -> frame
  in
  let m_inv = Matrix.inv (Frame.matrix frame) in
  let p = Matrix.apply m_inv p in
  touched_by t p

let max_diff vs ~f =
  match List.map vs ~f with
  | [] -> 0.
  | x :: xs ->
    let max = List.fold_left xs ~init:x ~f:Float.max in
    let min = List.fold_left xs ~init:x ~f:Float.min in
    max -. min

let width ~line_width ~touches = function
  | Rectangle       -> rect_width
  | Circle          -> circle_radius *. 2.
  | Horizontal_line -> 0.
  | Vertical_line   -> 0.
  | Cross_line      -> 0.
  | Zigzag          -> max_diff touches ~f:Vector.x +. (line_width *. 2.)
  | Bezier          -> max_diff touches ~f:Vector.x +. (line_width *. 2.)

let height ~line_width ~touches= function
  | Rectangle       -> rect_height
  | Circle          -> circle_radius *. 2.
  | Horizontal_line -> 0.
  | Vertical_line   -> 0.
  | Cross_line      -> 0.
  | Zigzag          -> max_diff touches ~f:Vector.y +. (line_width *. 2.)
  | Bezier          -> max_diff touches ~f:Vector.y +. (line_width *. 2.)

(* Assuming no scaling is already applied. *)
let scale_to_fit box size =
  let line_width = Box.line_width box in
  let t = Box.kind box in
  let touches = Box.touches box in
  let current_size =
    Float.max (width t ~line_width ~touches) (height t ~line_width ~touches)
  in
  let s = Float.min 1.0 (size /. current_size) in
  let frame = Frame.((Box.frame box) *> scale ~scale_x:s ~scale_y:s) in
  Box.set box ~frame

(*
  let to_string = function
  | Rectangle -> "Rectangle"
  | Circle -> "Circle"
  | Horizontal_line -> "Horz"
  | Vertical_line -> "Vert"
  | Cross_line -> "Cross"
  | Zigzag _ -> "Zigzag"
  | Bezier _ -> "Bezier"
*)
