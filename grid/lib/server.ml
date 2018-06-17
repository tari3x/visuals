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
  if config.screen_grid then return None
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
    let base_color = Config.start_color config in
    let grid =
      match config.grid_kind with
      | `grid ->
        let segments = Grid.Segments.Grid { cols = 7; rows = 3 } in
        Grid.create ~config ~ctx ~sound ~segments ?real_corners ~base_color ()
      | `free ->
        let svg = get_element_by_id "svg-iframe" Html.CoerceTo.iframe in
        let { Svg. segments; calibration_points } = Svg.parse_exn svg in
        let segments = Grid.Segments.Set segments in
        let native_corners = calibration_points in
        Grid.create
          ~config ~ctx ~sound ~segments
          ~native_corners ?real_corners ~base_color ()
    in
    Server_state.start config grid ~ctx
  end
