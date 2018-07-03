(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
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

let main (config : Config.t) =
  Random.self_init ();
  Sound.create_from_mic ()
  >>= fun sound ->
  let ctx = Ctx.create ~id:"main_canvas" in
  if config.debug_sound
  then begin
    Sound.set_debug sound (Some ctx);
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
