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
  debug [%message "333"];
  Window.set_reload_on_resize ();
  let box =
    Config.sparks config
    |> List.map ~f:Config.Sparks.skin
    |> List.map ~f:Spark.Ctl.set_config
    |> Ctl.list
    |> Box.create
  in
  let global_config =
    { Global.Config.viewport_width = 1.
    ; viewport_height = 1.
    ; is_server = false
    ; max_clients = 1
    ; max_box_age = Config.max_box_age config
    ; global_channel_name = config.global_channel_name
    }
  in
  let%bind global =
    Global.create_exn global_config ~sexp_of_a:Ctl.sexp_of_t
  in
  let box_id = Global.add global box in
  Global.delete global box_id;
  return ()
;;
