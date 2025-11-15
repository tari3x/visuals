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
  ; shapes : Shape.t list
  ; frames : int ref
  }

let color = Color.green |> Color.set_alpha ~alpha:0.

let rec fps_loop t =
  let%bind () = Lwt_js.sleep 1. in
  let frames = !(t.frames) in
  debug [%message (frames : int)];
  t.frames := 0;
  fps_loop t
;;

let render_loop { perspective; pixi; shapes; frames } =
  let rec loop () =
    let%bind () = Lwt_js_events.request_animation_frame () in
    Pixi.clear pixi;
    List.iter shapes ~f:(Shape.render ~perspective ~pixi ~color);
    Int.incr frames;
    loop ()
  in
  loop ()
;;

let main () =
  debug [%message "before init"];
  let%bind pixi = Pixi.init_exn () in
  debug [%message "init done"];
  let perspective = Matrix.ident in
  let rect width height =
    Rectangle.create_offset V.zero ~width ~height |> Rectangle.corner_list
  in
  let polygon =
    rect 100. 100.
    |> Geometry.Shape.polygon
    |> Shape.create ~line_width:10.
  in
  let path =
    rect 300. 300. |> Geometry.Shape.path |> Shape.create ~line_width:10.
  in
  let m =
    let open Matrix in
    rotate (Angle.of_degrees 45.)
    *> scale ~scale_x:1. ~scale_y:1.
    *> translate (Vector.create 500 500)
  in
  Shape.set_transform polygon m;
  Shape.set_transform path m;
  let t =
    { pixi; shapes = [ path; polygon ]; perspective; frames = ref 0 }
  in
  Lwt.async (fun () -> render_loop t);
  Lwt.async (fun () -> fps_loop t);
  Lwt.return ()
;;

let () = top_level (fun () -> main ())
