(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Js_of_ocaml_lwt
open Std_internal

type t =
  { sparks : Spark.t list
  ; global : Spark.Ctl.t Global.t
  ; ctx : Ctx.t
  }

let render t =
  Ctx.clear t.ctx;
  List.iter t.sparks ~f:(fun spark ->
    Global.iter t.global ~f:(Spark.ctl spark);
    Spark.render spark)

let rec render_loop t =
  Lwt_js_events.request_animation_frame ()
  (* Lwt_js.sleep 0.01 *)
  >>= fun () ->
  render t;
  render_loop t

let start (config : Config.t) sparks ~ctx =
  let global_config =
    { Global.Config.
      viewport_width = Ctx.width ctx
    ; viewport_height = Ctx.height ctx
    ; is_server = true
    ; max_clients = 6
    ; max_box_age = Config.max_box_age config
    ; global_channel_name = config.global_channel_name
    }
  in
  Global.create global_config ~sexp_of_a:Spark.Ctl.sexp_of_t
  >>= fun global ->
  let t = { sparks; global; ctx } in
  render t;
  (* Make sure to pick up changes from [Index_load]. *)
  Global.on_change global ~f:(fun _ _ -> render t);
  Lwt.async (fun () -> render_loop t);
  Lwt.return ()
