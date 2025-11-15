(*
   Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
   This file is distributed under a BSD license.
   See LICENSE file for copyright notice.
*)

open Core
open Lwt
open Js_of_ocaml_lwt
open Std_internal

type t =
  { config : Config.t
  ; sparks : Spark.t list
  ; global : Ctl.t Global.t option
  ; pixi : Pixi.t
  ; mutable frames : int
  ; mutable last_frame_report : Time_ns.t
  }

let render t =
  Pixi.clear t.pixi;
  let () =
    (* CR-someday: should I only be doing this in [on_change]? *)
    match t.global with
    | None -> ()
    | Some global ->
      Global.iter global ~f:(fun box ->
        debug [%message "global ctl"];
        match Box.kind box with
        | All ctl ->
          let box = Box.map box ~f:(const ctl) in
          List.iter t.sparks ~f:(fun spark -> Spark.ctl spark box)
        | List ctls ->
          List.iter2_exn t.sparks ctls ~f:(fun spark ctl ->
            let box = Box.map box ~f:(const ctl) in
            Spark.ctl spark box))
  in
  List.iter t.sparks ~f:Spark.render;
  let w = Pixi.width t.pixi in
  let h = Pixi.height t.pixi in
  (* CR-someday avatar: use [Graphics.rect] *)
  let fill x y w h =
    let v = Vector.create_float in
    let vs =
      Float.[ v x y; v x (y + h); v (x + w) (y + h); v (x + w) y ]
    in
    Pixi.path t.pixi ~closed:true vs;
    Pixi.fill t.pixi Color.black
  in
  fill 0. 0. w t.config.crop_top;
  fill 0. Float.(h - t.config.crop_bottom) w t.config.crop_bottom
;;

let one_sec = Time_ns.Span.of_sec 1.

let report_frame t =
  t.frames <- t.frames + 1;
  let now_ = Time_ns.now () in
  if Time_ns.(add t.last_frame_report one_sec < now_)
  then (
    debug [%message (t.frames : int)];
    t.frames <- 0;
    t.last_frame_report <- now_)
;;

let rec render_loop t =
  Lwt_js_events.request_animation_frame ()
  >>= fun () ->
  render t;
  report_frame t;
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
        Global.create_exn global_config ~sexp_of_a:Ctl.sexp_of_t
      in
      return (Some global))
    (function
      | exn ->
        debug [%message (exn : Exn.t)];
        return None)
  >>= fun global ->
  let t =
    { config
    ; sparks
    ; global
    ; pixi
    ; frames = 0
    ; last_frame_report = Time_ns.now ()
    }
  in
  render t;
  (* Make sure to pick up changes from [Index_load]. *)
  Option.iter global ~f:(Global.on_change ~f:(fun _ _ -> render t));
  Lwt.async (fun () -> render_loop t);
  Lwt.return ()
;;
