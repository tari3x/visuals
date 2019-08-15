(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)



open Base
open Lwt
open Js_of_ocaml_lwt
open Std_internal

let get_clicks ctx =
  (* NB! We assume that mouse click coordinates are the same as canvas
     coordinates. *)
  let clicks =
    Ctx.canvas_actions ctx
    |> Lwt_stream.filter_map ~f:Action.click
  in
  Lwt_stream.take clicks ~n:4

let get_corners (config : Config.t) ctx =
  if config.skip_calibration then return None
  else begin
    get_clicks ctx
    >>= fun clicks ->
    return (Some (Prism.Quad.of_list_exn clicks))
  end

let _test_quantum () =
  let open Lwt.Let_syntax in
  (* let quantum = 0.0001 in *)
  let count = ref 0 in
  let start = ref (Time.now ()) in
  let rec loop () =
    (* let%bind () = Lwt_js.sleep quantum in *)
    let%bind () = Lwt_js_events.request_animation_frame () in
    Int.incr count;
    if Time.(Span.(now () - !start > Time.Span.of_sec 1.))
    then begin
      debug [%message (!count : int)];
      start := Time.now ();
      count := 0
    end;
    loop ()
  in
  loop ()

let main (config : Config.t) =
  debug [%message "222"];
  Random.self_init ();
  Sound.create_from_mic ~max_sources:config.num_sound_sources
  >>= fun sound ->
  let ctx = Ctx.create ~id:"main_canvas" in
  if config.debug_sound
  then begin
    Sound.set_debug sound (Some ctx);
    Sound.start sound;
    return ()
  end
  else begin
    get_corners config ctx
    >>= fun real_corners ->
    let grid =
      match config.grid_kind with
      | `grid ->
        let shapes = Grid.Shapes.grid_exn ~cols:7 ~rows:3 in
        Grid.create ~config ~ctx ~sound ~shapes ?real_corners ()
      | `free ->
        let svg = get_element_by_id "svg-iframe" Html.CoerceTo.iframe in
        let { Svg. shapes; calibration_points } = Svg.parse_exn svg in
        let shapes = Grid.Shapes.set_exn shapes in
        let native_corners = calibration_points in
        Grid.create
          ~config ~ctx ~sound ~shapes
          ~native_corners ?real_corners ()
    in
    Server_state.start config grid ~ctx
  end
