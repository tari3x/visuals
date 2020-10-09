(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Core_kernel
open Lwt
open Js_of_ocaml_lwt
open Std_internal

type t =
  { sparks : Spark.t list
  ; global : Spark.Ctl.t Global.t option
  ; pixi : Pixi.t
  }

let render t =
  Pixi.clear t.pixi;
  List.iter t.sparks ~f:(fun spark ->
      Option.iter t.global ~f:(Global.iter ~f:(Spark.ctl spark));
      Spark.render spark)
;;

let rec render_loop t =
  Lwt_js_events.request_animation_frame ()
  (* Lwt_js.sleep 0.01 *)
  >>= fun () ->
  render t;
  render_loop t
;;

let start (config : Config.t) sparks ~pixi =
  let global_config =
    { Global.Config.viewport_width = Pixi.width pixi
    ; viewport_height = Pixi.height pixi
    ; is_server = true
    ; max_clients = 6
    ; max_box_age = Config.max_box_age config
    ; global_channel_name = config.global_channel_name
    }
  in
  Lwt.catch
    (fun () ->
      let%bind global =
        Global.create_exn global_config ~sexp_of_a:Spark.Ctl.sexp_of_t
      in
      return (Some global))
    (function
      | exn ->
        debug [%message (exn : Exn.t)];
        return None)
  >>= fun global ->
  let t = { sparks; global; pixi } in
  render t;
  (* Make sure to pick up changes from [Index_load]. *)
  Option.iter global ~f:(Global.on_change ~f:(fun _ _ -> render t));
  Lwt.async (fun () -> render_loop t);
  Lwt.return ()
;;
