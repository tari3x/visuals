(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Util
open Common
open Dom_wrappers
open Remote

module L = Lwt_stream

module State = State_light

type t = Grid.Ctl.t State.t

let main shape =
  Dom_wrappers.set_reload_on_resize ();
  Random.self_init ();
  let picker_ctx = Ctx.create ~id:"color-picker-canvas" in
  Color_picker.draw picker_ctx;
  let ctx = Ctx.create ~id:"main_canvas" in
  let actions = Ctx.canvas_actions ctx in
  State.create ctx shape ~sexp_of_a:Grid.Ctl.sexp_of_t
  >>= fun t ->
  Lwt.async (fun () -> Color_picker.run t picker_ctx);
  Lwt_stream.iter_with_try actions ~f:(State.process_action t)
