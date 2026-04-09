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
  ; sparks : Spark.t Spark.Id.Table.t
  ; global : Ctl.t Global.t option
  ; pixi : Pixi.t
  ; crop : Pixi.Graphics.t
  ; mutable frames : int
  ; mutable last_frame_report : Time_ns.t
  }

let render t =
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
          Hashtbl.iter t.sparks ~f:(fun spark -> Spark.ctl spark box)
        | List ctls ->
          Hashtbl.iteri ctls ~f:(fun ~key ~data:ctl ->
            let spark = Hashtbl.find_exn t.sparks key in
            let box = Box.map box ~f:(const ctl) in
            Spark.ctl spark box))
  in
  Hashtbl.iter t.sparks ~f:Spark.render;
  let w = Pixi.width t.pixi in
  let h = Pixi.height t.pixi in
  let module G = Pixi.Graphics in
  (* CR-someday avatar: use [Graphics.rect] *)
  let fill x y w h =
    let v = Vector.create_float in
    let vs =
      Float.[ v x y; v x (y + h); v (x + w) (y + h); v (x + w) y ]
    in
    G.path t.crop ~closed:true vs;
    G.fill t.crop Color.black;
    G.set_zindex t.crop 10000000
  in
  (* CR-someday avatar: don't clear, create once at the start *)
  G.clear t.crop;
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

(* CR-someday avatar: you should add a ticker to pixi instead of running your
   own loop. *)
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
  let crop = Pixi.create_graphics pixi in
  let t =
    { config
    ; sparks
    ; global
    ; pixi
    ; crop
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
