(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

open Lwt
open Lwt.Let_syntax

module State = State_light

let main (config : Config.t) =
  Window.set_reload_on_resize ();
  Random.self_init ();
  let svg = get_element_by_id "svg-iframe" Html.CoerceTo.iframe in
  let { Svg. shapes; calibration_points = _ } = Svg.parse_exn svg in
  let set_shapes = Grid.Ctl.Set_shapes shapes in
  let shape = Box.create ~kind:set_shapes () in
  let global_config =
    { Global.Config.
      viewport_width = 1.
    ; viewport_height = 1.
    ; is_server = false
    ; max_clients = 1
    ; max_box_age = Config.max_box_age config
    ; global_channel_name = config.global_channel_name
    }
  in
  let%bind global =
    Global.create global_config ~sexp_of_a:Grid.Ctl.sexp_of_t
  in
  let box_id = Global.add global shape in
  Global.delete global box_id;
  return ()
