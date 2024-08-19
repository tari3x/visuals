(*
   Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
   This file is distributed under a BSD license.
   See LICENSE file for copyright notice.
*)

open Base
open Js_of_ocaml_lwt
open Std_internal
module Shape = Spark_lib.Shapes.Elt

type t =
  { pixi : Pixi.t
  ; perspective : Matrix.t
  ; shape : Shape.t
  ; frames : int ref
  }

let color = Color.white

let rec fps_loop t =
  let%bind () = Lwt_js.sleep 1. in
  let frames = !(t.frames) in
  debug [%message (frames : int)];
  t.frames := 0;
  fps_loop t
;;

let render_loop { perspective; pixi; shape; frames } =
  let rec loop () =
    (* This makes it visibly slow: *)
    (* let%bind () = Lwt_js_events.request_animation_frame () in *)
    let%bind () = Lwt_js.sleep 0.01 in
    for _ = 1 to 5 do
      Shape.render shape ~perspective ~pixi ~color
    done;
    Int.incr frames;
    loop ()
  in
  loop ()
;;

let main () =
  let pixi = Pixi.init_exn () in
  let perspective = Matrix.ident in
  let shape =
    Rectangle.create_offset V.zero ~width:100. ~height:100.
    |> Rectangle.corner_list
    |> Geometry.Shape.polygon
    |> Shape.create ~line_width:10.
  in
  let m =
    let open Matrix in
    rotate (Angle.of_degrees 45.)
    *> scale ~scale_x:1. ~scale_y:0.1
    *> translate (Vector.create 500 500)
  in
  Shape.set_transform shape m;
  let t = { pixi; shape; perspective; frames = ref 0 } in
  Lwt.async (fun () -> render_loop t);
  Lwt.async (fun () -> fps_loop t);
  Lwt.return ()
;;

let () = top_level (fun () -> main ())
