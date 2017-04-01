(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common
open Geometry
open Dom_wrappers

module Quad = struct
  type t = Vector.t * Vector.t * Vector.t * Vector.t

  let create v1 v2 v3 v4 =
    (v1, v2, v3, v4)

  (* There is a canvas function for this, but setting it up would take more
     code. *)
  let contains (u0, u1, u2, u3) v =
    let rec angles = function
      | [] | [_] -> []
      | v1 :: v2 :: vs ->
        Angle.to_radians (Vector.angle v1 v2) :: angles (v2 :: vs)
    in
    let angle =
      List.map [u0; u1; u2; u3; u0] ~f:(fun u ->
        Vector.(u - v))
      |> angles
      |> List.fold_left ~init:0. ~f:(+.)
    in
    abs_float(angle) > 1.

  let draw (u0, u1, u2, u3) ~ctx ~color =
    Ctx.set_line_width ctx 1.;
    Ctx.set_stroke_color ctx color;
    Ctx.begin_path ctx;
    Ctx.move_to ctx u0;
    Ctx.line_to ctx u1;
    Ctx.line_to ctx u2;
    Ctx.line_to ctx u3;
    Ctx.line_to ctx u0;
    Ctx.stroke ctx
end

module Surface = struct
  type t =
    { canvas : Quad.t
    ; camera : Quad.t
    ; camera_to_canvas : Matrix.t
    }

  let create ~canvas ~camera =
    let open Vector in
    let (v0, v1, v2, v3) = canvas in
    let (u0, u1, u2, u3) = camera in
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
      (* transposed compared to the paper since we multiply the vectors from
         another side. *)
      Matrix.create
        ( x.(0), x.(1), x.(2) )
        ( x.(3), x.(4), x.(5) )
        ( x.(6), x.(7), 1. )
      |> Matrix.inv
    in
    { canvas; camera; camera_to_canvas }

  let camera_to_canvas t v =
    let v = Matrix.apply t.camera_to_canvas v in
    Option.some_if (Quad.contains t.canvas v) v

  let draw t ~ctx =
    (* Quad.draw t.camera ~ctx ~color:Color.green; *)
    Quad.draw t.canvas ~ctx ~color:Color.red
end

type t = Surface.t list

let create t =
  t

let camera_to_canvas t v =
  List.find_map t ~f:(fun surface ->
    Surface.camera_to_canvas surface v)

let draw t ~ctx =
  List.iter t ~f:(Surface.draw ~ctx)
