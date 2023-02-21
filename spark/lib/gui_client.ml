(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Std_internal
module L = Lwt_stream
module State = State_light

type t = Ctl.t State.t

let main (config : Config.t) shape =
  Window.set_reload_on_resize ();
  Random.self_init ();
  let color_picker =
    Color_picker.create
      { Color_picker.Config.include_black_strip = config.drawing_mode }
      (Ctx.create ~id:"color-picker-canvas")
  in
  Color_picker.draw color_picker;
  let ctx = Ctx.create ~id:"main_canvas" in
  (*
    let start_color =
    if Config.drawing_mode
    then Color.white
    else Color.black
    in
     let grid = Grid.create ~ctx ~sound ~rows:3 ~cols:7 ~color:start_color () in
     Grid.render grid;
  *)
  let actions = Ctx.canvas_actions ctx in
  State.create_exn
    { State.Config.max_box_age = Config.max_box_age config
    ; global_channel_name = config.global_channel_name
    }
    ctx
    shape
    ~sexp_of_a:Ctl.sexp_of_t
  >>= fun t ->
  Lwt.async (fun () -> Color_picker.run color_picker t);
  Lwt_stream.iter_with_try actions ~f:(State.process_action t)
;;
