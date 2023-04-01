(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

(* CR-someday: something in Std_internal shadows Fn.id *)
open Core

let hex_line_width = 3.
let grid_line_width = 3.

(* 18. for window *)
let shorten_by = 0. (* 7. *)

module Elt = struct
  module Id = struct
    include String

    let name = "Shape_id"
    let counter = ref 0
    let reset () = counter := 0

    let create () =
      incr counter;
      Printf.sprintf "%s%d" name !counter
    ;;
  end

  (* CR avatar: sort out sexp for setting shapes *)
  type t =
    { id : Id.t
    ; mutable shape : Shape.t
    ; mutable transform : (Pixi.Matrix.t[@sexp.opaque])
    ; line_width : float
    }
  [@@deriving fields, sexp]

  let create shape ~line_width =
    { id = Id.create ()
    ; shape
    ; transform = Pixi.Matrix.create Matrix.ident
    ; line_width
    }
  ;;

  (* CR avatar: is centre correct when transformation is present? *)
  let centre t = Shape.centre t.shape

  (* CR avatar: don't shorten in render, it's a memory leak. *)
  let _shortened_ends v1 v2 =
    let open Vector in
    let delta = normalize (v2 - v1) * shorten_by in
    v1 + delta, v2 - delta
  ;;

  (* CR avatar: perspective *)
  let render
    { id = _; shape; transform; line_width }
    ~perspective:_
    ~pixi
    ~color
    =
    match shape with
    | Segment (v1, v2) ->
      Pixi.set_matrix pixi transform;
      Pixi.line_style pixi ~color ~width:line_width ();
      Pixi.move_to pixi v1;
      Pixi.line_to pixi v2;
      Pixi.end_fill pixi
    | Polygon vs ->
      Pixi.set_matrix pixi transform;
      Pixi.line_style pixi ~width:0. ();
      Pixi.begin_fill pixi color;
      Pixi.path pixi ~closed:true vs;
      Pixi.end_fill pixi
    | Path vs ->
      Pixi.set_matrix pixi transform;
      Pixi.line_style pixi ~color ~width:line_width ();
      Pixi.path pixi ~closed:true vs;
      Pixi.end_fill pixi
  ;;

  let set_transform t m =
    let transform = Pixi.Matrix.create m in
    t.transform <- transform
  ;;
end

type t =
  { elts : Elt.t Elt.Id.Map.t
  ; corners : Prism.Quad.t
  ; step : float
  }
[@@deriving fields, sexp]

let create ~corners ~step ~line_width shapes =
  Elt.Id.reset ();
  let elts =
    List.map shapes ~f:(Elt.create ~line_width)
    |> List.map ~f:(fun elt -> Elt.id elt, elt)
    |> Elt.Id.Map.of_alist
    |> function
    | `Ok x -> x
    | `Duplicate_key key ->
      raise_s [%message "duplicate shape key" (key : Elt.Id.t)]
  in
  { corners; elts; step }
;;

let pixi_corners ?(wmargin = 0.) ?(hmargin = 0.) pixi =
  let wmargin = Pixi.width pixi *. wmargin in
  let hmargin = Pixi.height pixi *. hmargin in
  let width = Pixi.width pixi -. (2. *. wmargin) in
  let height = Pixi.height pixi -. (2. *. hmargin) in
  let top_left = V.create_float wmargin hmargin in
  let bottom_right = V.(top_left + create_float width height) in
  Rectangle.create_corners top_left bottom_right
;;

let create_with_pixi ~pixi ~step ~line_width shapes =
  let corners = pixi_corners pixi |> Prism.Quad.rectangle in
  create ~corners shapes ~step ~line_width
;;

let grid_exn ~pixi ~rows ~cols =
  let corners = pixi_corners pixi ~wmargin:0.1 ~hmargin:0.1 in
  let top_left = Rectangle.top_left corners in
  let width = Rectangle.width corners in
  let height = Rectangle.height corners in
  if rows <= 0 || cols <= 0 then failwith "rows or cols not positive";
  let dy = height /. float rows in
  let dx = width /. float cols in
  let shapes =
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
  let step = Float.min dx dy in
  create shapes ~corners ~step ~line_width:grid_line_width
;;

module Hex_kind = struct
  type t =
    | Bone
    | Wire
    | Tile

  let segment = function
    | [ v1; v2 ] -> Shape.segment v1 v2
    | _ -> raise_s [%message "not a segment"]
  ;;

  let make_shape = function
    | Bone -> segment
    | Wire -> Shape.path
    | Tile -> Shape.polygon
  ;;

  let margin = function
    | Wire | Bone -> hex_line_width /. 2.
    | Tile -> hex_line_width +. 10.
  ;;

  (* 9., 10., 20. *)
end

let hex_left_margin = 20.

(* https://www.redblobgames.com/grids/hexagons/

   r1 is half height.
   d_x is full width.
   d_y is vertical distance between two centers.
*)
let hex_exn ~pixi ~(kind : Hex_kind.t) ~r1 ~vertices =
  let open Float in
  let r2 = r1 - Hex_kind.margin kind in
  let d_x = r1 * sqrt 3. in
  let d_y = r1 * 1.5 in
  let hex i j =
    let c_y = float j * d_y in
    let c_x = float i * d_x in
    let c_x = if Int.(j % 2 = 0) then c_x else c_x - (d_x / 2.) in
    let c_x = c_x + hex_left_margin in
    let c = Vector.create_float c_x c_y in
    let vertex i =
      let angle_deg = (60. * float i) - 30. in
      let angle_rad = pi / 180. * angle_deg in
      let d_x = r2 * cos angle_rad in
      let d_y = r2 * sin angle_rad in
      V.(c + create_float d_x d_y)
    in
    let vs = List.map vertices ~f:vertex in
    [ Hex_kind.make_shape kind vs ]
  in
  let width = Pixi.width pixi in
  let height = Pixi.height pixi in
  debug [%message (width : float) (height : float)];
  if Float.(Pixi.width pixi = 0.) then assert false;
  let n_x =
    ((Pixi.width pixi - hex_left_margin) / d_x) + 0.51 |> Int.of_float
  in
  let n_y = ((Pixi.height pixi - r1) / d_y) + 1. |> Int.of_float in
  List.cartesian_product (List.range 1 n_x) (List.range 1 n_y)
  |> List.concat_map ~f:(fun (i, j) -> hex i j)
;;

let all_vertices = List.init 6 ~f:Fn.id

let r1 ~pixi ~r1_mult =
  let open Float in
  ((Pixi.width pixi / 2.) - hex_left_margin) * (r1_mult / sqrt 3.)
;;

let hex_wire_exn ~pixi ~r1_mult =
  let r1 = r1 ~pixi ~r1_mult in
  hex_exn ~pixi ~kind:Wire ~vertices:all_vertices ~r1
  |> create_with_pixi ~pixi ~step:r1 ~line_width:hex_line_width
;;

let hex_tile_exn ~pixi ~r1_mult =
  let r1 = r1 ~pixi ~r1_mult in
  hex_exn ~pixi ~kind:Tile ~vertices:all_vertices ~r1
  |> create_with_pixi ~pixi ~step:r1 ~line_width:hex_line_width
;;

let hex_bone_exn ~pixi ~r1_mult =
  let r1 = r1 ~pixi ~r1_mult in
  List.init 6 ~f:(fun i ->
    hex_exn ~pixi ~kind:Bone ~r1 ~vertices:[ i; i + 1 ])
  |> List.concat
  |> create_with_pixi ~pixi ~step:r1 ~line_width:hex_line_width
;;

let set_transform t m =
  let transform = Pixi.Matrix.create m in
  Map.iter t.elts ~f:(fun elt -> elt.transform <- transform)
;;

let update t_old t =
  let { elts; corners; step } = t in
  let elts =
    Map.to_alist elts
    |> List.map ~f:(fun (id, elt) ->
         let new_elt =
           Map.find t_old.elts id |> Option.value ~default:elt
         in
         new_elt.shape <- elt.shape;
         (* CR avatar: why not? It jumps. *)
         (* new_elt.transform <- elt.transform; *)
         id, new_elt)
    |> Elt.Id.Map.of_alist_exn
  in
  { elts; corners; step }
;;
