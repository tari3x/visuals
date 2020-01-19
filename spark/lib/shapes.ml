(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

(* window: 18.
   hex: 3., 7, or 13
*)
let line_width = 3. (* 18. for window *)

let shorten_by = 0. (* 7. *)

module Shape = struct
  include Shape

  let centre t =
    let open Vector in
    match t with
    | Segment (v1, v2) -> (v1 + v2) / 2.
    | Polygon vs | Path vs ->
      let n = List.length vs in
      List.reduce_exn vs ~f:( + ) / float n
  ;;

  let shortened_ends v1 v2 =
    let open Vector in
    let delta = normalize (v2 - v1) * shorten_by in
    v1 + delta, v2 - delta
  ;;

  let render t ~perspective ~ctx ~color =
    match t with
    | Segment (v1, v2) ->
      Ctx.set_stroke_color ctx color;
      Ctx.set_line_width ctx line_width;
      let v1, v2 = shortened_ends v1 v2 in
      let v1 = Matrix.apply perspective v1 in
      let v2 = Matrix.apply perspective v2 in
      Ctx.path ctx ~closed:false [ v1; v2 ];
      Ctx.stroke ctx
    | Polygon vs ->
      Ctx.set_fill_color ctx color;
      let vs = List.map vs ~f:(Matrix.apply perspective) in
      Ctx.path ctx ~closed:true vs;
      Ctx.fill ctx
    | Path vs ->
      Ctx.set_stroke_color ctx color;
      Ctx.set_line_width ctx line_width;
      let vs = List.map vs ~f:(Matrix.apply perspective) in
      Ctx.path ctx ~closed:true vs;
      Ctx.stroke ctx
  ;;
end

type t =
  { shapes : Shape.t list
  ; corners : Prism.Quad.t
  }
[@@deriving fields]

let create_exn ~corners shapes =
  if List.is_empty shapes then failwith "Shapes.set_exn: empty list";
  { corners; shapes }
;;

let ctx_corners ?(wmargin = 0.) ?(hmargin = 0.) ctx =
  let wmargin = Ctx.width ctx *. wmargin in
  let hmargin = Ctx.height ctx *. hmargin in
  let width = Ctx.width ctx -. (2. *. wmargin) in
  let height = Ctx.height ctx -. (2. *. hmargin) in
  let top_left = V.create_float wmargin hmargin in
  let bottom_right = V.(top_left + create_float width height) in
  Rectangle.create_corners top_left bottom_right
;;

let grid_exn ~ctx ~rows ~cols =
  let corners = ctx_corners ctx ~wmargin:0.1 ~hmargin:0.1 in
  let top_left = Rectangle.top_left corners in
  let width = Rectangle.width corners in
  let height = Rectangle.height corners in
  if rows <= 0 || cols <= 0 then failwith "rows or cols not positive";
  let shapes =
    let dy = height /. float rows in
    let dx = width /. float cols in
    let point row col =
      Vector.(top_left + create_float (float col *. dx) (float row *. dy))
    in
    let segment ~row ~col ~kind =
      let row', col' =
        match kind with
        | `vertical -> row + 1, col
        | `horizontal -> row, col + 1
      in
      if row' > rows || col' > cols
      then None
      else Some (Shape.segment (point row col) (point row' col'))
    in
    let make ~kind =
      List.init (rows + 1) ~f:(fun row ->
          List.init (cols + 1) ~f:(fun col -> segment ~row ~col ~kind))
      |> List.concat
      |> List.filter_opt
    in
    make ~kind:`horizontal @ make ~kind:`vertical
  in
  let corners =
    let w = width in
    let h = height in
    let tr dx dy = Vector.(top_left + Vector.create_float dx dy) in
    let v1 = tr 0. 0. in
    let v2 = tr w 0. in
    let v3 = tr w h in
    let v4 = tr 0. h in
    Prism.Quad.create v1 v2 v3 v4
  in
  { shapes; corners }
;;

module Hex_kind = struct
  type t =
    | Wire
    | Tile

  let make_shape = function
    | Wire -> Shape.path
    | Tile -> Shape.polygon
  ;;

  let margin = function
    | Wire -> line_width /. 2.
    | Tile -> line_width +. 10.
  ;;

  (* 9., 10., 20. *)
end

(* https://www.redblobgames.com/grids/hexagons/ *)
(* CR: better interface for splitting sound sources. *)
let hex_exn ~ctx ~(kind : Hex_kind.t) =
  let open Float in
  let r1 = Ctx.width ctx / 25. in
  let r2 = r1 - Hex_kind.margin kind in
  let d_x = r1 * sqrt 3. in
  let d_y = r1 * 1.5 in
  let hex i j =
    let c_y = float j * d_y in
    let c_x = float i * d_x in
    let c_x = if Int.(j % 2 = 0) then c_x else c_x - (d_x / 2.) in
    let c = Vector.create_float c_x c_y in
    let corner i =
      let angle_deg = (60. * float i) - 30. in
      let angle_rad = pi / 180. * angle_deg in
      let d_x = r2 * cos angle_rad in
      let d_y = r2 * sin angle_rad in
      V.(c + create_float d_x d_y)
    in
    let vs = List.init 6 ~f:corner in
    [ Hex_kind.make_shape kind vs ]
  in
  let n_x = (Ctx.width ctx / d_x) - 1. |> Int.of_float in
  let n_y = (Ctx.height ctx / d_y) - 1. |> Int.of_float in
  let shapes =
    List.cartesian_product (List.range 1 n_x) (List.range 1 n_y)
    |> List.concat_map ~f:(fun (i, j) -> hex i j)
  in
  let corners = ctx_corners ctx |> Prism.Quad.rectangle in
  { shapes; corners }
;;

let hex_wire_exn ~ctx = hex_exn ~ctx ~kind:Wire
let hex_tile_exn ~ctx = hex_exn ~ctx ~kind:Tile
