(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Std_internal

module L = Lwt_stream

module State = State_light

type t = Shape.t State.t

let shapes =
  let open Shape in
  [ Zigzag
  ; Vertical_line
  ; Rectangle
  ; Horizontal_line
  ; Circle
  ; Cross_line
  ; Bezier
  ]

let choose_shape (actions : Action.t Lwt_stream.t) ctx =
  (* CR-someday: do better estimate of the toolbar width. *)
  let width = Ctx.width ctx |> Int.of_float in
  let height = Ctx.height ctx |> Int.of_float in
  let hstep = width / List.length shapes in
  let color_cycle = Color_cycle.random_constant () in
  let touches =
    let p1 = Vector.create (-50) (50) in
    let p2 = Vector.create 0   (-50) in
    let p3 = Vector.create 50  50 in
    [ p1; p2; p3 ]
  in
  let shapes =
    List.mapi shapes ~f:(fun i kind ->
      let x = hstep * i + hstep / 2 in
      let y = height / 2 in
      let frame = Frame.translate (Vector.create x y) in
      let shape =
        Box.create ~kind ~frame ~color_cycle ~line_width:10. ()
        |> Box.set_touches ~touches ~coordinates:`internal
      in
      let margin = min 50 (hstep / 5) in
      let clip_size = min hstep height - margin in
      let clip_p =
        Vector.create
          (hstep * i + (margin / 2))
          ((height - clip_size) / 2)
      in
      Ctx.save ctx;
      Ctx.clip_rect ctx clip_p ~width:clip_size ~height:clip_size;
      let shape = Shape.scale_to_fit shape (Float.of_int clip_size) in
      (* CR-someday: need late time *)
      Shape.render shape ctx ~time:100.;
      Ctx.restore ctx;
      shape
    )
  in
  Lwt_stream.find actions ~f:(fun action ->
    Action.Kind.equal action.kind `down)
  >>= fun action ->
  let p = Action.coords action in
  let n = Int.of_float (Vector.x p) / hstep in
  Ctx.clear ctx;
  let shape =
    List.nth_exn shapes n
    |> Box.set ~line_width:Box.default_line_width
  in
  Lwt.return (shape, action)

let main () =
  Window.set_reload_on_resize ();
  Random.self_init ();
  let color_picker =
    Color_picker.create
      { Color_picker.Config.
        include_black_strip = false
      }
      (Ctx.create ~id:"color-picker-canvas")
  in
  Color_picker.draw color_picker;
  let ctx = Ctx.create ~id:"main_canvas" in
  let actions = Ctx.canvas_actions ctx in
  choose_shape actions ctx
  >>= fun (shape, action) ->
  let state_config =
    { State.Config.
      max_box_age = Time.Span.of_sec 30.
    ; global_channel_name = "global"
    }
  in
  State.create state_config ctx shape ~sexp_of_a:Shape.sexp_of_t
  >>= fun t ->
  State.process_action t action;
  Lwt.async (fun () -> Color_picker.run color_picker t);
  Lwt_stream.iter_with_try actions ~f:(State.process_action t)
