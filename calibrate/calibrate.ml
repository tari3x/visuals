(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Common
open Geometry
open Dom_wrappers

(* Clockwise starting from top left (zero is top-left corner). *)
let v0 = Vector.create 400 200
let v1 = Vector.create 1000 200
let v2 = Vector.create 1000 800
let v3 = Vector.create 400 800

let markers = [ v0; v1; v2; v3 ]

let draw_markers ?(m = Matrix.ident) ctx =
  Ctx.clear ctx;
  Ctx.set_fill_color ctx Color.white;
  List.iter markers ~f:(fun v ->
    let v = Matrix.apply m v in
    debug "drawing marker %s" (Vector.to_string v);
    Ctx.draw_circle ctx v ~radius:2.)

let get_markers ctx =
  let actions = Ctx.canvas_actions ctx in
  let rec loop markers =
    Lwt_stream.next actions
    >>= fun action ->
    let markers =
      if action.kind = `down
      then begin
        let p = Action.coords action in
        markers @ [ p ]
      end
      else markers
    in
    match markers with
    | [u0; u1; u2; u3] ->
      (* These are correct:
         List.iter [u0; u1; u2; u3]s ~f:(fun v ->
         debug "%s" (Vector.to_string v));
      *)
      Lwt.return (u0, u1, u2, u3)
    | _ -> loop markers
  in
  loop []

let canvas_to_camera ctx =
  get_markers ctx
  >>= fun (u0, u1, u2, u3) ->
  let open Vector in
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
  (* transposed compared to the paper since we multiply the vectors from
     another side. *)
  Matrix.create
    ( x.(0), x.(1), x.(2) )
    ( x.(3), x.(4), x.(5) )
    ( x.(6), x.(7), 1. )
  |> Lwt.return

