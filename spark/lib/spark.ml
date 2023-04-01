(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Core
open Js_of_ocaml_lwt
open Std_internal
module Config = Config.Spark

(* TODO:
   - defining segment groups that flash together.
   - unify the straight lines in the picture. Not sure which way is better.
   - two independent projectors
*)

module CF = Color_flow
module Shape = Shapes.Elt

let max_keep_raining_probability = 1.

module Ctl = struct
  type t =
    | Spot
    | Rain_control
    | Set_config of Config.t
  [@@deriving sexp, variants]
end

type t =
  { mutable config : Config.t
  ; sound : Sound.t
  ; pixi : Pixi.t
  ; real_corners : Prism.Quad.t
  ; mutable perspective : Matrix.t
  ; skin : Skin.t
  ; mutable shapes : Shapes.t
  ; mutable last_transform_time : Time.t
  ; speeds : float Shape.Id.Table.t
  ; angles : Angle.t Shape.Id.Table.t
  ; mutable last_angle : Angle.t
  }

let perspective ~native_corners ~real_corners =
  Prism.Surface.create ~canvas:real_corners ~camera:native_corners
  |> Prism.Surface.camera_to_canvas
;;

let set_shapes t shapes =
  t.shapes <- Shapes.update t.shapes shapes;
  Skin.set_shapes t.skin shapes;
  Hashtbl.clear t.speeds;
  let native_corners = Shapes.corners shapes in
  t.perspective <- perspective ~native_corners ~real_corners:t.real_corners
;;

let make_speed (speed : Config.Speed.t) =
  let open Float in
  match speed with
  | Range { min; max } -> fun () -> min + Random.float (max - min)
  | List speeds ->
    let speeds = ref speeds in
    fun () ->
      (match !speeds with
       | [] -> assert false
       | [ x ] -> x
       | x :: xs ->
         speeds := xs;
         x)
;;

let start_transform_loop t =
  let center =
    let open Float in
    Vector.create_float (Pixi.width t.pixi / 2.) (Pixi.height t.pixi / 2.)
  in
  let rec loop () =
    let%bind () = Lwt_js_events.request_animation_frame () in
    match t.config with
    | Free _ | Grid _ | Hex_tile _ | Hex_wire _ | Hex_bone _ -> loop ()
    | Star { skin = _; shapes = _; speed; line_width = _ } ->
      let now_ = Time.now () in
      let d = Time.(now_ - t.last_transform_time) |> Time.Span.to_sec in
      t.last_transform_time <- now_;
      let make_speed = Memo.unit (fun () -> make_speed speed) in
      Shapes.elts t.shapes
      |> Map.iteri ~f:(fun ~key ~data:shape ->
           let open Float in
           let speed =
             Hashtbl.find_or_add t.speeds key ~default:(fun () ->
               make_speed () ())
           in
           let d = d * speed in
           let angle =
             match Hashtbl.find t.angles key with
             | None -> t.last_angle
             | Some angle -> Angle.(angle + of_degrees d)
           in
           t.last_angle <- angle;
           Hashtbl.set t.angles ~key ~data:angle;
           let m =
             let open Matrix in
             translate center * rotate angle
           in
           Shape.set_transform shape m);
      loop ()
  in
  Lwt.async loop
;;

let shapes (config : Config.t) ~pixi =
  match config with
  | Grid { skin = _; rows; cols } -> Shapes.grid_exn ~pixi ~cols ~rows
  | Hex_tile { skin = _; r1_mult } -> Shapes.hex_tile_exn ~pixi ~r1_mult
  | Hex_wire { skin = _; r1_mult } -> Shapes.hex_wire_exn ~pixi ~r1_mult
  | Hex_bone { skin = _; r1_mult } -> Shapes.hex_bone_exn ~pixi ~r1_mult
  | Free { skin = _; shapes } -> shapes
  | Star { skin = _; shapes; speed = _; line_width } ->
    Shapes.create_with_pixi ~pixi ~step:100. shapes ~line_width
;;

let create ~(config : Config.t) ~pixi ~sound ?real_corners () =
  let shapes = shapes config ~pixi in
  let native_corners = Shapes.corners shapes in
  let real_corners = Option.value real_corners ~default:native_corners in
  let perspective = perspective ~native_corners ~real_corners in
  (* elts are not empty by interface *)
  let skin =
    let config = Config.skin config in
    Skin.create ~config ~sound shapes
  in
  let t =
    { config
    ; pixi
    ; perspective
    ; shapes
    ; skin
    ; sound
    ; real_corners
    ; last_transform_time = Time.now ()
    ; speeds = Shape.Id.Table.create ()
    ; angles = Shape.Id.Table.create ()
    ; last_angle = Angle.zero
    }
  in
  start_transform_loop t;
  t
;;

(* This used to try and find the distance to the actual line rather than to
     the centre. Not sure what the point was, try the new method out. *)
let distance_to_point shape ~point =
  let open Vector in
  length (Shape.centre shape - point)
;;

let ctl t (box : Ctl.t Box.t) =
  debug [%message "ctl"];
  match Box.kind box with
  | Spot ->
    let color = Box.color box in
    let touches = Box.touches box ~coordinates:`canvas in
    List.filter_map touches ~f:(fun point ->
      Shapes.elts t.shapes
      |> Map.data
      |> List.min_elt ~compare:(fun s1 s2 ->
           Float.compare
             (distance_to_point ~point s1)
             (distance_to_point ~point s2)))
    |> List.dedup_and_sort ~compare:Poly.compare
    |> List.iter ~f:(fun elt -> Skin.human_touch t.skin elt color)
  | Rain_control ->
    (match Box.touches box ~coordinates:`canvas with
     | [] -> debug [%message "Rain_control with no touches!"]
     | touch :: _ ->
       let open Float in
       let x, y = Vector.coords touch in
       let w = Pixi.width t.pixi in
       let h = Pixi.height t.pixi in
       let skin_config = Config.skin t.config in
       skin_config.bot_active <- x / w > 0.5;
       skin_config.rain.keep_raining_probability
         <- max_keep_raining_probability * (y / h))
  | Set_config config ->
    t.config <- config;
    let shapes = shapes config ~pixi:t.pixi in
    set_shapes t shapes;
    Skin.set_config t.skin (Config.skin config)
;;

let render { skin; perspective; pixi; _ } =
  Skin.render skin ~perspective ~pixi
;;
