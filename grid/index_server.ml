(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Util
open Common
open Dom_wrappers

let get_clicks ctx =
    (* NB! We assume that mouse click coordinates are the same as canvas
       coordinates. *)
  let clicks =
    Ctx.canvas_actions ctx
    |> Lwt_stream.filter_map ~f:Action.click
  in
  Lwt_stream.take clicks ~n:4

let main () =
  let ctx = Ctx.create ~id:"main_canvas" in
  get_clicks ctx
  >>= fun clicks ->
  let corners = Prism.Quad.of_list_exn clicks in
  let grid = Grid.create ~ctx ~cols:7 ~rows:3 ~corners in
  Server_state.start grid ~ctx
;;

top_level main
