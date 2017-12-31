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

let main () =
  Sound.create_from_mic ()
  >>= fun sound ->
  let ctx = Ctx.create ~id:"main_canvas" in
  if Config.debug_sound
  then Sound.set_debug sound (Some ctx);
  get_clicks ctx
  >>= fun clicks ->
  let corners = Prism.Quad.of_list_exn clicks in
  let grid =
    Grid.create ~ctx ~sound ~cols:7 ~rows:3 ~corners ~color:start_color ()
  in
  Server_state.start grid ~ctx
;;

top_level main
