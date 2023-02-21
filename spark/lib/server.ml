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

let full_screen_x = 1707.
let full_screen_y = 1133.

let rectangle_quad width height =
  Rectangle.create_offset V.zero ~width ~height |> Prism.Quad.rectangle
;;

(* CR-someday: unify this with the logic for grid *)
let full_screen_laptop_corners = rectangle_quad full_screen_x full_screen_y

let get_corners (config : Config.t) pixi =
  let open Float in
  match config.calibration with
  | Laptop_aspect_ratio ->
    let x = Pixi.width pixi in
    let y = Pixi.height pixi in
    let a = min (x / full_screen_x) (y / full_screen_y) in
    return (Some (rectangle_quad (a * full_screen_x) (a * full_screen_y)))
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
    Sound.create_from_mic ~max_sources:(Config.num_sound_sources config)
  in
  (*
  let sound =
    Sound.create_from_html
      ~id:"audio"
      ~max_sources:config.num_sound_sources
  in
  *)
  let pixi = Pixi.init_exn () in
  if config.debug_sound
  then
    failwith "Fix debug sound"
    (*
    Sound.set_debug sound (Some pixi);
    Sound.start sound;
    return ()
    *)
  else
    get_corners config pixi
    >>= fun real_corners ->
    let shapess =
      List.concat_map config.sparks ~f:(function
          | Grid { skin; rows; cols } ->
            [ Shapes.grid_exn ~pixi ~cols ~rows, skin ]
          | Hex { tile_skin; wire_skin; r1_mult } ->
            let tile =
              Option.map tile_skin ~f:(fun tile_skin ->
                  Shapes.hex_tile_exn ~pixi ~r1_mult, tile_skin)
            in
            let wire = Shapes.hex_wire_exn ~pixi ~r1_mult, wire_skin in
            List.filter_opt [ tile; Some wire ]
          | Free skin ->
            let svg =
              get_element_by_id "svg-iframe" Html.CoerceTo.iframe
            in
            let { Svg.shapes; calibration_points } = Svg.parse_exn svg in
            let corners =
              match config.calibration with
              | Laptop_aspect_ratio -> full_screen_laptop_corners
              | Skip | Clicks -> calibration_points
            in
            [ Shapes.create_exn ~corners shapes, skin ])
    in
    let sparks =
      List.map shapess ~f:(fun (shapes, config) ->
          Spark.create ~config ~pixi ~sound ~shapes ?real_corners ())
    in
    Server_state.start config sparks ~pixi
;;
