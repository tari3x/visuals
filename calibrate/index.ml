(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Common
open Geometry
open Dom_wrappers

(* TODO:
   - make the center of the prism the center of rotation
   - figure out why there is a scroll bar at the bottom
   - correct the shape of circles? Set transform on context?
*)

let draw_marker ctx v =
  Ctx.set_fill_color ctx Color.white;
  Ctx.draw_circle ctx v ~radius:10.

let draw_rotating_grid ~prism ~ctx =
  Ctx.clear ctx;
  let rec loop angle =
    let angle = Angle.(angle + of_degrees 1.) in
    let r = Matrix.rotate angle in
    let t =
      Vector.create_float (Ctx.width ctx) (Ctx.height ctx)
      |> Vector.scale ~by:0.5
      |> Matrix.translate
    in
    let m = Matrix.(t * r) in
    Ctx.clear ctx;
    (* Prism.draw_border prism ~ctx; *)
    for i = -50 to 50 do
      for j = -50 to 50 do
        Vector.create (30 * i) (30 * j)
        |> Matrix.apply m
        |> Prism.camera_vector_to_canvas prism
        |> Option.iter ~f:(Ctx.draw_circle ctx ~radius:5.)
      done
    done;
    Lwt_js.sleep 0.04
    >>= fun () ->
    loop angle
  in
  loop Angle.zero

let draw_image ~prism ~ctx ~pos =
  Image.load "honeycomb.png"
  >>= fun image ->
  let image = Image_source.image image in
  Prism.camera_image_to_canvas prism ~image ~ctx ~pos;
  Prism.draw_border prism ~ctx;
  return ()

module Markers = struct
  type t =
    { camera : Vector.t list
    ; canvas : Vector.t list
    }

  let get ~ctx ~video n =
    Video.read_camera video;
    (* NB! We assume that mouse click coordinates are the same as canvas
       coordinates. *)
    let clicks =
      Ctx.canvas_actions ctx
      |> Lwt_stream.filter_map ~f:Action.click
    in
    (* You must start with the centre. *)
    Lwt_stream.take clicks ~n
    >>= fun canvas ->
    List.iter canvas ~f:(draw_marker ctx);
    Video.get_frame video ctx ~delay:true
    >>= fun () ->
    Lwt_stream.take clicks ~n
    >>= fun camera ->
    return { canvas; camera }

  let to_prism t indices =
    let s i = List.nth t.canvas i in
    let d i = List.nth t.camera i in
    List.map indices ~f:(fun (i1, i2, i3, i4) ->
      let canvas = Prism.Quad.create (s i1) (s i2) (s i3) (s i4) in
      let camera = Prism.Quad.create (d i1) (d i2) (d i3) (d i4) in
      Prism.Surface.create ~canvas ~camera)
    |> Prism.create
end

let corner_prism ~video ~ctx =
  Markers.get ~video ~ctx 7
  >>= fun markers ->
  let prism =
    Markers.to_prism markers
      [ (0, 1, 2, 3)
      ; (0, 3, 4, 5)
      ; (0, 5, 6, 1)
      ]
  in
  return (prism, List.nth markers.camera 0)

let flat_prism ~video ~ctx =
  Markers.get ~video ~ctx 4
  >>= fun markers ->
  let prism =
    Markers.to_prism markers [ (0, 1, 2, 3) ]
  in
  return (prism, List.nth markers.camera 0)

let main () =
  let video = Video.create ~id:"video" in
  let ctx = Ctx.create ~id:"canvas" in
  flat_prism ~video ~ctx
  >>= fun (prism, pos) ->
  draw_image ~prism ~ctx ~pos
;;

top_level main
