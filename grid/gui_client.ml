(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Std_internal

let start_color =
  if Config.drawing_mode
  then Color.white
  else Color.black

module L = Lwt_stream

module State = State_light

type t = Grid.Ctl.t State.t

let main shape =
  Window.set_reload_on_resize ();
  Random.self_init ();
  let picker_ctx = Ctx.create ~id:"color-picker-canvas" in
  Color_picker.draw picker_ctx;
  let ctx = Ctx.create ~id:"main_canvas" in
  (*
     let grid = Grid.create ~ctx ~sound ~rows:3 ~cols:7 ~color:start_color () in
     Grid.render grid;
  *)
  let actions = Ctx.canvas_actions ctx in
  State.create ctx shape ~sexp_of_a:Grid.Ctl.sexp_of_t
  >>= fun t ->
  Lwt.async (fun () -> Color_picker.run t picker_ctx);
  Lwt_stream.iter_with_try actions ~f:(State.process_action t)
