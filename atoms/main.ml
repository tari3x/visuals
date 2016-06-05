open Lwt
open Js
open Common

let line_width = 5.
let dot_radius = 200.

let setup_canvas id =
  let canvas = get_element_by_id id Html.CoerceTo.canvas in
  let ctx = canvas##getContext Html._2d_ in
  canvas##.width := Html.document##.body##.clientWidth;
  canvas##.height := Html.document##.body##.clientHeight;
  canvas, ctx

module Color = struct
  type t =
    { r : int
    ; g : int
    ; b : int
    ; a : float
    }

  let wave ~cycle time =
    ((sin (time *. cycle) +. 1.) /. 2.) *. 255. |> int

  let of_time time =
    let r = wave ~cycle:1. time in
    let g = wave ~cycle:2. time in
    let b = wave ~cycle:3. time in
    let a = 0.7 in
    { r; g; b; a }

  let to_string { r; g; b; a } =
    Printf.sprintf "rgba(%d, %d, %d, %f)" r g b a
end

let setup_spotlight () =
  let canvas, ctx = setup_canvas "spotlight_canvas" in
  let dot_active = ref false in
  let horizontal_active = ref false in
  let vertical_active = ref false in
  let redraw p =
    let time = Sys.time () in
    let color time = Color.of_time time |> Color.to_string |> string in
    clear ctx;
    ctx##.fillStyle := (color time);
    if !dot_active then draw_dot ctx p dot_radius;
     ctx##.fillStyle := (color (1.41 *. time));
    if !horizontal_active then draw_horizontal_line ctx p line_width ();
     ctx##.fillStyle := (color (1.78 *. time));
    if !vertical_active then draw_vertical_line ctx p line_width ();
  in
  let mouse_switch ev value =
    begin match Html.buttonPressed ev with
    | Html.No_button     -> ()
    | Html.Left_button   -> horizontal_active := value;
    | Html.Middle_button -> dot_active := value
    | Html.Right_button  -> vertical_active := value
    end;
    redraw (Mouse_event.client_coords ev)
  in
  let touch_switch ev value =
    horizontal_active := value;
    redraw (Touch_event.client_coords ev)
  in
  add_event_listener canvas Html.Event.mousedown ~f:(fun ev ->
    mouse_switch ev true);
  add_event_listener canvas Html.Event.mouseup ~f:(fun ev ->
    mouse_switch ev false);
  add_event_listener canvas Html.Event.mousemove ~f:(fun ev ->
    redraw (Mouse_event.client_coords ev));
  add_event_listener canvas Html.Event.touchstart ~f:(fun ev ->
    touch_switch ev true)

let main () =
  (*
  let canvas, ctx = setup_canvas "image_canvas" in
  load_image "image.png"
  >>= fun image ->
  ctx##drawImage_withSize image
    0. 0.
    (float canvas##.clientWidth) (float canvas##.clientHeight);
  *)
  setup_spotlight ();
  Lwt.return ()

let go _ = ignore (
  catch (fun () -> main ())
    (fun exn -> error "uncaught exception: %s" (Printexc.to_string exn)));
  _true

;;

Html.window##.onload := Html.handler go

