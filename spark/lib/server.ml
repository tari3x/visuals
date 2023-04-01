(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Js_of_ocaml_lwt
open Std_internal

let get_clicks pixi =
  (* NB! We assume that mouse click coordinates are the same as canvas
     coordinates. *)
  let clicks =
    Pixi.actions pixi |> Lwt_stream.filter_map ~f:Action.click
  in
  Lwt_stream.take clicks ~n:4
;;

let rectangle_quad width height =
  Prism.Quad.create_offset V.zero ~width ~height
;;

let get_corners (config : Config.t) pixi =
  let open Float in
  match config.calibration with
  | Aspect_ratio { x = screen_x; y = screen_y } ->
    let x = Pixi.width pixi in
    let y = Pixi.height pixi in
    let a = min (x / screen_x) (y / screen_y) in
    return (Some (rectangle_quad (a * screen_x) (a * screen_y)))
  | Clicks ->
    get_clicks pixi
    >>= fun clicks -> return (Some (Prism.Quad.of_list_exn clicks))
  | Skip -> return None
;;

let _test_quantum () =
  let open Lwt.Let_syntax in
  (* let quantum = 0.0001 in *)
  let count = ref 0 in
  let start = ref (Time.now ()) in
  let rec loop () =
    (* let%bind () = Lwt_js.sleep quantum in *)
    let%bind () = Lwt_js_events.request_animation_frame () in
    Int.incr count;
    if Time.(Span.(now () - !start > Time.Span.of_sec 1.))
    then (
      debug [%message (!count : int)];
      start := Time.now ();
      count := 0);
    loop ()
  in
  loop ()
;;

let main (config : Config.t) =
  debug [%message "333"];
  Config.validate config;
  Random.self_init ();
  let%bind sound =
    (*
    let%bind () = Lwt_js.sleep 3. in
    Sound.create_from_html
      ~id:"audio"
      ~max_sources:(Config.num_sound_sources config)
    |> return
    *)
    Sound.create_from_mic ()
  in
  Sound.start sound;
  let pixi = Pixi.init_exn () in
  if config.debug_sound
  then
    failwith "Fix debug sound"
    (*
    Sound.set_debug sound (Some pixi);
    Sound.start sound;
    return ()
    *)
  else (
    let%bind real_corners = get_corners config pixi in
    let sparks =
      List.map config.sparks ~f:(fun config ->
        Spark.create ~config ~pixi ~sound ?real_corners ())
    in
    Server_state.start config sparks ~pixi)
;;
