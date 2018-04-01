(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Std_internal

let start_color =
  if Config.drawing_mode
  then Color.black
  else Color.white

let get_clicks ctx =
  (* NB! We assume that mouse click coordinates are the same as canvas
     coordinates. *)
  let clicks =
    Ctx.canvas_actions ctx
    |> Lwt_stream.filter_map ~f:Action.click
  in
  Lwt_stream.take clicks ~n:4

let get_corners ctx =
  if Config.screen_grid then return None
  else begin
    get_clicks ctx
    >>= fun clicks ->
    return (Some (Prism.Quad.of_list_exn clicks))
  end

let main () =
  Random.self_init ();
  Sound.create_from_mic ()
  >>= fun sound ->
  let ctx = Ctx.create ~id:"main_canvas" in
  if Config.debug_sound
  then Sound.set_debug sound (Some ctx);
  get_corners ctx
  >>= fun real_corners ->
  let color = start_color in
  let grid =
    match Config.grid_kind with
    | `grid ->
      let segments = Grid.Segments.Grid { cols = 7; rows = 3 } in
      Grid.create ~ctx ~sound ~segments ?real_corners ~color ()
    | `free ->
      let svg = get_element_by_id "svg-iframe" Html.CoerceTo.iframe in
      let { Svg. segments; calibration_points } = Svg.parse_exn svg in
      let segments = Grid.Segments.Set segments in
      let native_corners = calibration_points in
      Grid.create ~ctx ~sound ~segments ~native_corners ?real_corners ~color ()
  in
  Server_state.start grid ~ctx
;;

top_level main
