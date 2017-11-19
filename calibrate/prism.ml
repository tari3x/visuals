(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Util
open Common
open Geometry
open Dom_wrappers

module Quad = struct
  type t = Vector.t * Vector.t * Vector.t * Vector.t

  let create v1 v2 v3 v4 =
    (v1, v2, v3, v4)

  let transform (v1, v2, v3, v4) ~by:m =
    let f v = Matrix.apply m v in
    (f v1, f v2, f v3, f v4)

  (* There is a canvas function for this, but setting it up would take more
     code. *)
  let contains (u0, u1, u2, u3) v =
    let rec angles = function
      | [] | [_] -> []
      | v1 :: v2 :: vs ->
        Angle.to_radians (Vector.angle v1 v2) :: angles (v2 :: vs)
    in
    let angle =
      List.map [u0; u1; u2; u3; u0] ~f:(fun u -> Vector.(u - v))
      |> angles
      |> List.fold ~init:0. ~f:(+.)
    in
    Float.(abs angle > 1.)

  let path (u0, u1, u2, u3) ctx =
    Ctx.begin_path ctx;
    Ctx.move_to ctx u0;
    Ctx.line_to ctx u1;
    Ctx.line_to ctx u2;
    Ctx.line_to ctx u3;
    Ctx.line_to ctx u0

  let draw_border t ~ctx ~color =
    Ctx.set_line_width ctx 1.;
    Ctx.set_stroke_color ctx color;
    path t ctx;
    Ctx.stroke ctx

  let geometry (u0, u1, u2, u3) ~canvas =
    let open Three_wrappers in
    let uvm = Matrix.scale
      ~scale_x:(1. /. (Canvas.width canvas))
      ~scale_y:(1. /. (Canvas.height canvas))
    in
    let uv0 = Matrix.apply uvm u0 in
    let uv1 = Matrix.apply uvm u1 in
    let uv2 = Matrix.apply uvm u2 in
    let uv3 = Matrix.apply uvm u3 in
    let vertices = Array.map [| u0; u1; u2; u3 |] ~f:Vector3.of_vector in
    Geometry.create ()
      ~vertices
      ~faces:[| Face.create (0, 1, 2) ~uvs:(uv0, uv1, uv2)
             ;  Face.create (2, 3, 0) ~uvs:(uv2, uv3, uv0)
             |]

  let mesh ~texture ?color ~canvas t =
    let open Three in
    let open Three_wrappers in
    let geometry = geometry t ~canvas in
    let material = MeshBasicMaterial.create ~map:texture ?color () in
    material##.side := Material.Side.double_side;
    material##.wireframe := Js._true;
    material##.transparent := Js._true;
    let obj = Mesh.create geometry (material :> Three.Material.t) in
    (obj :> Object3D.t)
end

module Surface = struct
  type t =
    { canvas : Quad.t
    ; camera : Quad.t
    ; camera_to_canvas : Matrix.t
    }

  let create ~canvas ~camera =
    let open Vector in
    let (v0, v1, v2, v3) = camera in
    let (u0, u1, u2, u3) = canvas in
    (* "Fundamentals of Texture Mapping and Image Warping",
       "Inferring Projective Mappings" *)
    let a =
      [| [| (x v0); (y v0); 1.; 0.; 0.; 0.; -.(x v0) *. (x u0); -.(y v0) *. (x u0) |];
         [| (x v1); (y v1); 1.; 0.; 0.; 0.; -.(x v1) *. (x u1); -.(y v1) *. (x u1) |];
         [| (x v2); (y v2); 1.; 0.; 0.; 0.; -.(x v2) *. (x u2); -.(y v2) *. (x u2) |];
         [| (x v3); (y v3); 1.; 0.; 0.; 0.; -.(x v3) *. (x u3); -.(y v3) *. (x u3) |];

         [| 0.; 0.; 0.; (x v0); (y v0); 1.; -.(x v0) *. (y u0); -.(y v0) *. (y u0) |];
         [| 0.; 0.; 0.; (x v1); (y v1); 1.; -.(x v1) *. (y u1); -.(y v1) *. (y u1) |];
         [| 0.; 0.; 0.; (x v2); (y v2); 1.; -.(x v2) *. (y u2); -.(y v2) *. (y u2) |];
         [| 0.; 0.; 0.; (x v3); (y v3); 1.; -.(x v3) *. (y u3); -.(y v3) *. (y u3) |]
      |]
      |> Math.Matrix.of_array
    in
    let b =
      [| x u0; x u1; x u2; x u3; y u0; y u1; y u2; y u3 |]
      |> Math.Vector.of_array
    in
    let x =
      Math.lusolve a b
      |> Math.flatten
      |> Math.Vector.to_array
    in
    let camera_to_canvas =
      (* transposed compared to the paper since we multiply vectors from another
         side. *)
      Matrix.create
        ( x.(0), x.(1), x.(2) )
        ( x.(3), x.(4), x.(5) )
        ( x.(6), x.(7), 1. )
    in
    { canvas; camera; camera_to_canvas }

  let camera_vector_to_canvas t v =
    let v = Matrix.apply t.camera_to_canvas v in
    Option.some_if (Quad.contains t.canvas v) v

  let draw_camera_image_on_canvas t ~texture ~(scene : Three.Scene.t) ~canvas =
    let open Three_wrappers in
    let camera_mesh =
      Quad.mesh t.camera ~texture ~color:Color.green ~canvas
    in
    scene##add camera_mesh;
    let canvas_mesh =
      Quad.mesh t.canvas ~texture ~color:Color.red ~canvas
    in
    scene##add canvas_mesh;
    let camera_on_canvas_direct_mesh =
      Quad.transform t.camera ~by:t.camera_to_canvas
      |> Quad.mesh ~texture ~color:Color.blue ~canvas
    in
    scene##add camera_on_canvas_direct_mesh;
    let camera_on_canvas_mesh = Quad.mesh t.camera ~texture ~canvas in
    (* obj##applyMatrix (Matrix4.of_matrix t.camera_to_canvas); *)
    debug_table t.camera_to_canvas;
    (*
       let m = Matrix4.of_matrix t.camera_to_canvas in
       debug_table (m##.elements);
    *)
    Object3D.set_matrix camera_on_canvas_mesh (Matrix4.of_matrix t.camera_to_canvas);
    scene##add camera_on_canvas_mesh

  let draw_border t ~ctx =
    Quad.draw_border t.camera ~ctx ~color:Color.green;
    Quad.draw_border t.canvas ~ctx ~color:Color.red
end

type t = Surface.t list

let create t =
  t

let camera_vector_to_canvas t v =
  List.find_map t ~f:(fun surface ->
    Surface.camera_vector_to_canvas surface v)

let draw_camera_image_on_canvas t ~texture ~scene ~canvas =
  List.iter t ~f:(Surface.draw_camera_image_on_canvas ~texture ~scene ~canvas)

let draw_border t ~ctx =
  List.iter t ~f:(Surface.draw_border ~ctx)
