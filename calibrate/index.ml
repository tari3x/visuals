(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Common
open Geometry
open Dom_wrappers

(*
  - collect some number of points, right click terminates
  - photo
  - collect clicks in the same order
  - start drawing
*)

let main () =
  let video = Video.create ~id:"video" in
  Video.read_camera video;
  let ctx = Ctx.create ~id:"canvas" in
  Calibrate.draw_markers ctx;
  Video.get_frame video ctx ~delay:true
  >>= fun () ->
  Calibrate.canvas_to_camera ctx
  >>= fun m ->
  let m = Matrix.inv m in
  debug "%s" (Matrix.to_string m);
  Ctx.clear ctx;
  let rec loop angle =
    let angle = Angle.(angle + of_degrees 1.) in
    let r = Matrix.rotate angle in
    let t =
      Vector.create_float (Ctx.width ctx) (Ctx.height ctx)
      |> Vector.scale ~by:0.5
      |> Matrix.translate
    in
    let m = Matrix.(m * t * r) in
    Ctx.clear ctx;
    for i = -50 to 50 do
      for j = -50 to 50 do
        Vector.create (100 * i) (100 * j)
        |> Matrix.apply m
        |> Ctx.draw_circle ctx ~radius:5.
      done
    done;
    Lwt_js.sleep 0.04
    >>= fun () ->
    loop angle
  in
  loop Angle.zero
(*
  (* We should see a regular grid in the photo now. *)
  Video.get_frame video ctx ~delay:true
*)
;;

top_level main
