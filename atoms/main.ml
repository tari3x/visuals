open Lwt
open Js
open Common

let line_width = 5.
let dot_radius = 200.

type t = State.t

let add_handlers t =
  let open State in
  let canvas = canvas t in
  add_event_listener canvas Html.Event.mousedown ~f:(fun ev ->
    process_action t (Action.of_mouse_event ev `down));
  add_event_listener canvas Html.Event.mouseup ~f:(fun ev ->
    process_action t (Action.of_mouse_event ev `up));
  add_event_listener canvas Html.Event.mousemove ~f:(fun ev ->
    process_action t (Action.of_mouse_event ev `move));
  add_event_listener canvas Html.Event.touchstart ~f:(fun ev ->
    Dom.preventDefault ev;
    process_action t (Action.of_touch_event ev `down));
  add_event_listener canvas Html.Event.touchend ~f:(fun ev ->
    Dom.preventDefault ev;
    process_action t (Action.of_touch_event ev `up));
  add_event_listener canvas Html.Event.touchmove ~f:(fun ev ->
    Dom.preventDefault ev;
    process_action t (Action.of_touch_event ev `move))

let main ~is_leader =
  (*
  let canvas, ctx = setup_canvas "image_canvas" in
  load_image "image.png"
  >>= fun image ->
  ctx##drawImage_withSize image
    0. 0.
    (float canvas##.clientWidth) (float canvas##.clientHeight);
  *)
  let t = State.create ~is_leader in
  add_handlers t;
  Lwt.return ()

let go ~is_leader _ = ignore (
  catch (fun () -> main ~is_leader)
    (fun exn -> error "uncaught exn: %s" (Printexc.to_string exn)));
  _true

;;
