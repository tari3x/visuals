(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Js
open Common
open Geometry
open Action

module Button = struct
  type t = [ `left | `right | `middle | `touch | `none ]
end

let is_inside v (target : #Html.element Js.t) =
  let open Float in
  let x, y = Vector.coords v in
  0. <= x && x <= (float target##.clientWidth)
  && 0. <= y && y <= (float target##.clientHeight)

module Mouse_event = struct
  type t = Dom_html.mouseEvent Js.t

  (* CR: There is an experimental [offsetX] on mouse events, but isn't supported
     in js_of_ocaml. Try going unsafe? *)
  let client_coords target t =
    Vector.create
      (t##.clientX - target##.offsetLeft)
      (t##.clientY - target##.offsetTop)

  (* At some point I made a note that the touchpad is always left on move! Not
     only that, it is also down! But now I can't reproduce it.
  *)
  let button t : Button.t =
    match Html.buttonPressed t with
    | Html.No_button     -> `none
    | Html.Left_button   -> `left
    | Html.Middle_button -> `middle
    | Html.Right_button  -> `right

  let action (target : #Html.element Js.t) (t : t) kind =
    let position = client_coords target t in
    let button   = button t in
    let id = Pointer_id.create 1 in
    let changed_touches =
      if not (is_inside position target) then []
      else [ { Pointer. id; position } ]
    in
    { Action. kind; changed_touches; button }
end

(* CR: make it work with multi-touch. *)
module Touch_event = struct
  type t = Dom_html.touchEvent Js.t

  let action (target : #Html.element Js.t) (t : t) kind =
    let changed_touches = t##.changedTouches in
    let changed_touches =
      List.init (changed_touches##.length) ~f:(fun i -> changed_touches##item i)
      |> List.filter_map ~f:(fun touch ->
        let touch = Optdef.value_exn touch in
        let position =
          Vector.create
            (touch##.clientX - target##.offsetLeft)
            (touch##.clientY - target##.offsetTop)
        in
        let id = Pointer_id.create touch##.identifier in
        if not (is_inside position target) then None
        else Some { Pointer. id; position })
    in
    { Action. kind; changed_touches; button = `touch }
end

let actions (target : #Html.element Js.t) =
  let stream, write = Lwt_stream.create () in
  ignore (target##.offsetLeft);
  let write x = write (Some x) in
  add_event_listener target Html.Event.mousedown ~f:(fun ev ->
    write (Mouse_event.action target ev `down));
  add_event_listener target Html.Event.mouseup ~f:(fun ev ->
    write (Mouse_event.action target ev `up));
  add_event_listener target Html.Event.mousemove ~f:(fun ev ->
    write (Mouse_event.action target ev `move));
  add_event_listener target Html.Event.touchstart ~f:(fun ev ->
    Dom.preventDefault ev;
    write (Touch_event.action target ev `down));
  add_event_listener target Html.Event.touchend ~f:(fun ev ->
    Dom.preventDefault ev;
    write (Touch_event.action target ev `up));
  add_event_listener target Html.Event.touchmove ~f:(fun ev ->
    Dom.preventDefault ev;
    write (Touch_event.action target ev `move));
  stream

module Image = struct
  type t = Html.imageElement Js.t

  let load src =
    let t = Html.createImg Html.document in
    Lwt.wrap
      (fun c ->
        t##.onload := Html.handler (fun _ -> c (); Js._false);
        t##.src := (string src))
    >>= fun () ->
    Lwt.return t
end

module Image_source = struct
  type t =
  [ `image  of Image.t
  | `canvas of Html.canvasElement Js.t
  | `video  of Html.videoElement  Js.t
  ]

  let image  x = `image x
  let canvas x = `canvas x
  let video  x = `video x
end

module Canvas = struct
  type t = Html.canvasElement Js.t

  let width t =
    Float.of_int t##.width

  let height t =
    Float.of_int t##.height
end

module Ctx = struct
  type t = Html.canvasRenderingContext2D Js.t

  let create ~id =
    let canvas = get_element_by_id id Html.CoerceTo.canvas in
    (* Set the internal size equal to display size. *)
    canvas##.width := canvas##.offsetWidth;
    canvas##.height := canvas##.offsetHeight;
    canvas##getContext Html._2d_

  let canvas t =
    t##.canvas

  let canvas_actions t =
    actions (t##.canvas)

  let width t =
    t##.canvas##.clientWidth |> float

  let height t =
    t##.canvas##.clientHeight |> float

  let clear t =
    t##clearRect 0. 0. (width t) (height t)

  let draw_circle (t : t) p ~radius =
    t##beginPath;
    t##arc (Vector.x p) (Vector.y p) radius 0. (2. *. Js.math##._PI) _false;
    t##fill

  let move_to (t : t) p =
    let x, y = Vector.coords p in
    t##moveTo x y

  let line_to (t : t) p =
    let x, y = Vector.coords p in
    t##lineTo x y

  let bezier_curve_to (t : t) ~control1 ~control2 v =
    let x1, y1 = Vector.coords control1 in
    let x2, y2 = Vector.coords control2 in
    let x, y   = Vector.coords v in
    t##bezierCurveTo x1 y1 x2 y2 x y

  let begin_path (t : t) =
    t##beginPath

  let stroke (t : t) =
    t##stroke

  let clip (t : t) =
    t##clip

  let set_fill_color (t : t) color =
    t##.fillStyle := string (Color.to_string color)

  let set_stroke_color (t : t) color =
    t##.strokeStyle := string (Color.to_string color)

  let set_line_width (t : t) width =
    t##.lineWidth := width

  let set_line_join (t : t) style =
    let style =
      match style with
      | `round -> "round"
      | `bevel -> "bevel"
      | `miter -> "miter"
    in
    t##.lineJoin := (string style)

  let set_line_cap (t : t) style =
    let style =
      match style with
      | `butt -> "butt"
      | `round -> "round"
      | `square -> "square"
    in
    t##.lineCap := (string style)

  let set_font t font =
    t##.font := (string font)

  let fill_text (t : t) text x y =
    t##fillText (string text) x y

  let stroke_text (t : t) text x y =
    t##strokeText (string text) x y

  let save (t : t) =
    t##save

  let restore (t : t) =
    t##restore

  let set_transform (t : t) m =
    Matrix.suitable_for_context2d_exn m;
    let m = Matrix.coeffs m in
    let c i j = m.(i).(j) in
    t##setTransform
      (c 0 0) (c 1 0)
      (c 0 1) (c 1 1)
      (c 0 2) (c 1 2)

  let transform (t : t) m =
    Matrix.suitable_for_context2d_exn m;
    let m = Matrix.coeffs m in
    let c i j = m.(i).(j) in
    t##transform
      (c 0 0) (c 1 0)
      (c 0 1) (c 1 1)
      (c 0 2) (c 1 2)

  let stroke_without_transform (t : t) =
    save t;
    set_transform t Matrix.ident;
    t##stroke;
    restore t

  let draw_horizontal_line (t : t) p =
    let y = Vector.y p in
    t##save;
    t##beginPath;
    t##moveTo (-10_000.) y;
    t##lineTo 10_000. y;
    stroke_without_transform t;
    t##restore

  let draw_vertical_line (t : t) p =
    let x = Vector.x p in
    t##save;
    t##beginPath;
    t##moveTo x (-10_000.);
    t##lineTo x 10_000.;
    stroke_without_transform t;
    t##restore

  let clip_rect (t : t) p ~width ~height =
    let (x, y) = Vector.coords p in
    t##beginPath;
    t##rect x y (Float.of_int width) (Float.of_int height);
    t##clip

  let fill_rect t v ~width ~height =
    let x, y = Vector.coords v in
    t##fillRect x y width height

  let fill_all t =
    fill_rect t Vector.zero ~width:(width t) ~height:(height t)

  let draw_centered_rectangle (t : t) ~width ~height =
    t##fillRect
      ((-.width) /. 2.)
      ((-.height) /. 2.)
      width height

  let draw (t : t) image p =
    let x, y = Vector.coords p in
    match image with
    | `image  image  -> t##drawImage                    image  x y
    | `canvas canvas -> t##drawImage_fromCanvas         canvas x y
    | `video  video  -> t##drawImage_fromVideoWithVideo video  x y
end

module Video = struct
  type t = Html.videoElement Js.t

  let create ~id =
    get_element_by_id id Html.CoerceTo.video

  let read_camera t =
    Js_std.read_camera t

  let rec get_frame ?(delay = false) (t : t) (ctx : Ctx.t) =
    begin
      if not delay then Lwt.return () else Lwt_js.sleep 3.
    end
    >>= fun () ->
    Lwt_js_events.request_animation_frame ()
    >>= fun () ->
    Ctx.clear ctx;
    match t##.readyState with
    | Html.HAVE_ENOUGH_DATA ->
      ctx##drawImage_fromVideoWithVideo t 0. 0.;
      Lwt.return ()
    | _ ->
      get_frame t ctx ~delay:false

  (* CR-someday: wait for the canplay event? *)
  let load ?(loop = false) src =
    let t = Html.createVideo Html.document in
    t##.src := (Js.string src);
    t##.loop := Js.bool loop;
    (* t##load; *)
    t##play;
    (* t##.autoplay := Js._true; *)
    t
end

module Audio = struct
  type t = Html.audioElement Js.t

  let create ~id =
    get_element_by_id id Html.CoerceTo.audio
end

module Window = struct
  let set_reload_on_resize () =
    add_event_listener Html.window Html.Event.resize ~f:(fun _ ->
      Html.window##.location##reload)
end
