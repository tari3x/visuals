(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Util
open Common
open Geometry
open Dom_wrappers

(* TODO:
   - make the center of the prism the center of rotation
   - figure out why there is a scroll bar at the bottom
   - correct the shape of circles? Set transform on context?
   - we don't seem to be taking up the whole area lit by the projector.
   - the vidoe is larger than the projector, change resolution or someothing.
*)

(* CR: is it dropping when determinant is negative or something? *)
(* CR: capture a bad example (sexp?) *)
(* CR: trace calls to webgl *)
(* CR: can we restrict m_11 to be positive? *)
(* CR: how does the convexity of points come in?
   Multiplying directly gives the right quad!
*)

let apply_canvas_style (element : Html.element Js.t) =
  let style = element##.style in
  style##.position := Js.string "absolute";
  style##.width := Js.string "100%";
  style##.height := Js.string "100%"

let draw_marker ctx v =
  Ctx.set_fill_color ctx Color.white;
  Ctx.draw_circle ctx v ~radius:3.

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

let rec render renderer scene camera =
  Lwt_js_events.request_animation_frame ()
  >>= fun () ->
  renderer##render scene camera;
  return ()
(* render renderer scene camera *)

let test_draw_honeycomb () =
  let open Three in
  let open Three_wrappers in
  (* Texture.load_image "images/honeycomb.png" *)
  Texture.load_video "images/rotating-cube.mp4"
  >>= fun texture ->
  let scene = Scene.create () in
  let camera = OrthographicCamera.create_with_body_size () in
  let renderer = WebGLRenderer.create_with_body_size () in
  (* renderer##setClearColor (Color.create 0. 1. 0.); *)
  let canvas = renderer##.domElement in
  let (_ : Dom.node Js.t) =
    Html.document##.body##appendChild (canvas :> Dom.node Js.t)
  in
  let v = Vector3.create in
  let vertices =
    [| v 0.   0. 0.
    ;  v 0.  500. 0.
    ;  v 500.  0. 0.
    ;  v 500. 500. 0.
    |]
  in
  (* CR: fix these if using texture. *)
  let uv0 = Vector.create_float 0. 0. in
  let uv1 = Vector.create_float 0. 1. in
  let uv2 = Vector.create_float 1. 0. in
  let uv3 = Vector.create_float 1. 1. in
  let geometry =
    Geometry.create ()
      ~vertices
      ~faces:[| Face.create (0, 1, 2) ~uvs:(uv0, uv1, uv2) ~color:Color.blue
             ;  Face.create (1, 3, 2) ~uvs:(uv1, uv3, uv2) ~color:Color.green
             |]
  in
  let material =
    MeshBasicMaterial.create ~color:Color.red ~map:texture ()
  in
  material##.side := Material.Side.double_side;
  Firebug.console##log material;
  let obj = Mesh.create geometry (material :> Three.Material.t) in
  scene##add (obj :> Object3D.t);
  render renderer scene (camera :> Camera.t)

let draw_texture ~prism ~pos:_ ~texture =
  let open Three in
  let open Three_wrappers in
  let scene = Scene.create () in
  let camera = OrthographicCamera.create_with_body_size () in
  let renderer = WebGLRenderer.create_with_body_size () in
  (* renderer##setClearColor (Color.create 1. 0. 0.); *)
  let canvas = renderer##.domElement in
  apply_canvas_style (canvas :> Html.element Js.t);
  Dom.appendChild Html.document##.body canvas;
  Prism.draw_camera_image_on_canvas prism ~texture ~scene ~canvas;
  render renderer scene (camera :> Camera.t)

module Markers = struct
  type t =
    { camera : Vector.t list
    ; canvas : Vector.t list
    }

  let rectangle =
    let v = Vector.create in
    let camera =
      [ v 100 100
      ; v 100 200
      ; v 200 200
      ; v 200 100
      ]
    in
    { camera; canvas = camera }

  (* CR: what's the story with non-convex? *)
  let trapezoid ~delta =
    let v = Vector.create in
    let quad ~delta ~blup =
      [ v (210 + delta) 60
      ; v (100 - delta) 100
      ; v (300 + delta) (100 - blup)
      ; v (300 - delta) 60
      ]
      |> List.map ~f:(Vector.scale ~by:4.)
    in
    let camera = quad ~blup:10 ~delta:(- delta) in
    let canvas = quad ~blup:0 ~delta in
    { camera; canvas }

  let get ~ctx ?video n =
    Option.iter video ~f:Video.read_camera;
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
    begin match video with
    | None -> return ()
    | Some video -> Video.get_frame video ctx ~delay:true
    end
    >>= fun () ->
    Lwt_stream.take clicks ~n
    >>= fun camera ->
    return { canvas; camera }

  let to_prism t indices =
    let s i = List.nth_exn t.canvas i in
    let d i = List.nth_exn t.camera i in
    List.map indices ~f:(fun (i1, i2, i3, i4) ->
      let canvas = Prism.Quad.create (s i1) (s i2) (s i3) (s i4) in
      let camera = Prism.Quad.create (d i1) (d i2) (d i3) (d i4) in
      Prism.Surface.create ~canvas ~camera)
    |> Prism.create
end

let corner_prism ?video ~ctx () =
  Markers.get ?video ~ctx 7
  >>= fun markers ->
  let prism =
    Markers.to_prism markers
      [ (0, 1, 2, 3)
      ; (0, 3, 4, 5)
      ; (0, 5, 6, 1)
      ]
  in
  return (prism, List.nth markers.camera 0)

let flat_prism ?video ~ctx () =
  Markers.get ?video ~ctx 4
  >>= fun markers ->
  let prism =
    Markers.to_prism markers [ (0, 1, 2, 3) ]
  in
  return (prism, List.nth markers.camera 0)

let test_prism () =
  let markers = Markers.trapezoid ~delta:30 in
  let prism =
    Markers.to_prism markers [ (0, 1, 2, 3) ]
  in
  return (prism, List.nth markers.camera 0)

let main () =
  let open Three_wrappers in
  (* test_draw_honeycomb () *)
  let ctx = Ctx.create ~id:"canvas" in
  let video = Video.create ~id:"video" in
  (* test_prism () *)
  flat_prism (* ~video *) ~ctx ()
  (* corner_prism (* ~video *) ~ctx () *)
  >>= fun (prism, pos) ->
  Dom.removeChild Html.document##.body (Ctx.canvas ctx);
  (* CR: your video is not great, perspective is already distorted. *)
  (* CR: cpu is pegging. Compare different three.js versions. chrome does a lot
     better than firefox, although still clearly there is a busy loop
     somewhere. Doesn't it peg even without video? *)
  (* Texture.load_video "images/rotating-cube.mp4" ~loop:true *)
  Texture.load_image "images/honeycomb.png"
  >>= fun texture ->
  draw_texture ~prism ~pos ~texture;
  (*
  let v1 = Vector.create 50 10 in
  let v2 = Vector.create 50 300 in
  let v3 = Vector.create 300 0 in
  let v4 = Vector.create 300 300 in
  let surface =
    let canvas = Prism.Quad.create v1 v2 v3 v4 in
    let camera = Prism.Quad.create v1 v2 v3 v4 in
    Prism.Surface.create ~canvas ~camera
  in
  let prism = Prism.create [ surface ] in
  draw_image ~prism ~ctx ~pos:(Vector.create 0 0)
  *)
;;

top_level main
