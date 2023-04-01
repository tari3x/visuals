(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Js_of_ocaml_lwt
open Std_internal
open Lwt.Let_syntax

(* TODO:

   * borders?
   * try tweaking centre max weight
   * try using manhattan distance or similar

   * try with chrome

   * smaller triangles

   * 3d elements, like actual cubes with sparks flowing over.

   * Check why drawing on 2D canvas so CPU intensive.
   Set alpha: false in [getContext] to try improving performance.

   * Why does fred seem to flash so much more than the sound debug?

   * read up about sampling

   * detangle continuing rain from beats, stop the rain at the border,
   for instance.

   * less bright cyan
*)

module CF = Color_flow
module PD = Probability_distribution
module V = Vector
module Config = Config.Skin
module Shape = Shapes.Elt

module E = struct
  module Id = Shape.Id

  type t =
    { mutable config : Config.t
    ; shape : Shape.t
    ; mutable color_flow : Color_flow.t option
    ; mutable base_color : Color.t
    }
  [@@deriving fields]

  let id t = Shape.id t.shape
  let a c alpha = Color.set_alpha c ~alpha
  let current_color t = Option.map t.color_flow ~f:CF.eval

  let touch t new_color =
    let set () = t.color_flow <- Some new_color in
    match t.color_flow with
    | None -> set ()
    | Some old_color ->
      let old_color = CF.eval old_color in
      let new_color = CF.eval new_color in
      (* Transitions "down" look ragged, avoid. *)
      if Float.(Color.a new_color >= Color.a old_color) then set ()
  ;;

  let fade_to_black ~(config : Config.t) ~base_color ~flash =
    let open Time.Span in
    let sls = config.segment_life_span in
    if not flash
    then
      CF.start_now (a base_color config.flash_cutoff)
      |> CF.add ~after:(sls * 0.5) ~color:(a base_color 0.)
    else
      CF.start_now (a base_color config.flash_top)
      |> CF.add
           ~after:(sls * config.flash_duration)
           ~color:(a base_color config.flash_cutoff)
      |> CF.add
           ~after:(sls * Float.(1. - config.flash_duration))
           ~color:(a base_color 0.)
  ;;

  let fade_to_black_smooth
    ~(config : Config.t)
    ~start_color
    ~end_color
    ~flash
    =
    let open Float in
    let open Time.Span in
    let sls = config.segment_life_span in
    if not flash
    then
      CF.start_now start_color
      |> CF.add ~after:(sls * 0.5) ~color:(a end_color config.flash_cutoff)
      |> CF.add ~after:(sls * 0.5) ~color:(a end_color 0.)
    else (
      let flash_step = config.flash_duration / 2.5 in
      CF.start_now start_color
      |> CF.add
           ~after:(sls * flash_step)
           ~color:(a end_color config.flash_top)
      |> CF.add
           ~after:(sls * Float.(0.5 * flash_step))
           ~color:(a end_color config.flash_top)
      |> CF.add
           ~after:(sls * flash_step)
           ~color:(a end_color config.flash_cutoff)
      |> CF.add
           ~after:(sls * Float.(1. - config.flash_duration))
           ~color:(a end_color 0.))
  ;;

  (* Always flashes *)
  let fade_to_base ~config ~flash_color ~new_base =
    let open Time.Span in
    let sls = Config.segment_life_span config in
    let flash_color = Color.maximize flash_color in
    (* Tried having an afterglow, didn't work, is just distracting. *)
    CF.start_now (a flash_color config.flash_top)
    |> CF.add
         ~after:(sls * config.flash_duration)
         ~color:(a new_base config.flash_cutoff)
  ;;

  let touch t ~color ~flash =
    (* CR-someday: if we are faded out, why interpolate? *)
    let base_color, color =
      match t.config.color_flow with
      | Fade_to_none ->
        let base_color =
          Color.interpolate [ t.base_color; color ] ~arg:0.5
        in
        let color = fade_to_black ~config:t.config ~base_color ~flash in
        base_color, color
      | Fade_to_none_smooth ->
        let base_color =
          Color.interpolate [ t.base_color; color ] ~arg:0.5
        in
        let start_color =
          Option.value (current_color t) ~default:Color.none
        in
        let color =
          fade_to_black_smooth
            ~config:t.config
            ~start_color
            ~end_color:base_color
            ~flash
        in
        base_color, color
      | Fade_to_base ->
        let new_base =
          Color.interpolate
            [ t.base_color; color ]
            ~arg:t.config.rain.fade_to_base_interpolation_arg
        in
        let flash_color =
          (* t.base_color *)
          Color.interpolate
            [ t.base_color; color ]
            ~arg:t.config.flash_color_weight
        in
        let color = fade_to_base ~config:t.config ~flash_color ~new_base in
        new_base, color
    in
    t.base_color <- base_color;
    touch t color
  ;;

  let create shape ~(config : Config.t) =
    let base_color = config.base_color in
    let t = { base_color; shape; config; color_flow = None } in
    touch t ~color:base_color ~flash:false;
    t
  ;;

  let centre t = Shape.centre t.shape

  let offset t1 t2 =
    let open Vector in
    centre t1 - centre t2
  ;;

  let render t ~perspective ~pixi =
    let open Float in
    match t.color_flow with
    | None -> ()
    | Some cf ->
      let color = CF.eval cf in
      if CF.finished cf && Color.a color = 0. then t.color_flow <- None;
      Shape.render t.shape ~perspective ~pixi ~color
  ;;
end

module Rain = Rain.Make (E)

type t =
  { mutable config : (Config.t[@sexp.opaque])
  ; mutable shapes : (Shapes.t[@sexp.opaque])
  ; elts : (E.t Hashtbl.M(E.Id).t[@sexp.opaque] (* not empty *))
  ; sound : (Sound.t[@sexp.opaque])
  ; silent_rains : Rain.t Hashtbl.M(Rain.Id).t
  ; sound_rains : Rain.t Hashtbl.M(Rain.Id).t
  ; sound_rain_ids : Rain.Id.t Hashtbl.M(Sound.Source.Id).t
  ; mutable last_human_touch : Time.t
  ; mutable silent_drop_count : int
  ; mutable num_sources : int
  ; mutable listener : (Sound.Listener.t[@sexp.opaque]) option
  }
[@@deriving sexp_of]

let reset_shapes t ~old_shapes (shapes : Shapes.t) =
  let new_shapes = Shapes.elts shapes in
  Map.symmetric_diff old_shapes new_shapes ~data_equal:(fun _ _ -> true)
  |> Sequence.iter ~f:(fun (key, value) ->
       match value with
       | `Unequal _ -> assert false
       | `Right elt ->
         let data = E.create elt ~config:t.config in
         Hashtbl.set t.elts ~key ~data
       | `Left _ -> Hashtbl.remove t.elts key);
  t.shapes <- shapes
;;

let set_shapes t shapes =
  let old_shapes = Shapes.elts t.shapes in
  reset_shapes t ~old_shapes shapes
;;

let create ~(config : Config.t) ~sound shapes =
  debug [%message "creating skin"];
  let last_human_touch =
    Time.(sub (now ()) config.human_playing_timeout)
  in
  debug [%message "human touch"];
  let t =
    { config
    ; sound
    ; silent_rains = Hashtbl.create (module Rain.Id)
    ; sound_rains = Hashtbl.create (module Rain.Id)
    ; sound_rain_ids = Hashtbl.create (module Sound.Source.Id)
    ; shapes
    ; elts = Hashtbl.create (module E.Id)
    ; last_human_touch
    ; silent_drop_count = 0
    ; num_sources = min 1 config.max_sound_sources
    ; listener = None
    }
  in
  debug [%message "created skin"];
  reset_shapes t ~old_shapes:Shapes.Elt.Id.Map.empty shapes;
  t
;;

let human_playing t =
  let open Time in
  let open Span in
  now () - t.last_human_touch < t.config.human_playing_timeout
;;

let rains t = Hashtbl.data t.silent_rains @ Hashtbl.data t.sound_rains

let new_rain t id =
  let step = Shapes.step t.shapes in
  Rain.create_exn
    ~id
    ~config:t.config.rain
    ~step
    ~other_rains:(rains t)
    ~elts:(Hashtbl.data t.elts)
;;

let find_or_add_rain t which ~id =
  let table =
    match which with
    | `silent -> t.silent_rains
    | `sound -> t.sound_rains
  in
  Hashtbl.find_or_add table id ~default:(fun () -> new_rain t id)
;;

(* No other function should be creating rains. *)
let run_silent_rain t =
  let rec loop rain =
    let%bind () = Lwt_js.sleep 15. in
    (* CR avatar: undo *)
    if (* Rain.saturation rain < 0.0001 *) false
    then loop rain
    else (
      Hashtbl.remove t.silent_rains (Rain.id rain);
      start_new ())
  and start_new () =
    let id = Rain.Id.create () in
    loop (find_or_add_rain t `silent ~id)
  in
  start_new ()
;;

let drop ?(flash = true) t =
  (* CR-someday: why silent? *)
  t.silent_drop_count <- t.silent_drop_count + 1;
  Option.iter (List.random_element (rains t)) ~f:(Rain.drop ~flash)
;;

let _run_silent_drops t =
  let open Lwt.Let_syntax in
  let open Float in
  (* CR-someday: surely there's something smarter possible that doesn't wake
       up every quantum if not many drops are happening. *)
  (* CR-someday: shurely the probability needs to scale with [quantum]? *)
  let quantum = 0.1 (* 0.001 *) in
  let drops =
    Resonant_flow.create_exn
      ~period_range:t.config.drops_period_range
      ~value_range:t.config.drops_value_range
  in
  let rec loop () =
    let%bind () = Lwt_js.sleep quantum in
    let drop_probability = Resonant_flow.eval drops in
    if Random.float 1. <= drop_probability then drop t;
    loop ()
  in
  loop ()
;;

let start_silent_rains t =
  if t.config.num_silent_rains > 0
  then
    for _ = 1 to t.config.num_silent_rains do
      Lwt.async (fun () -> run_silent_rain t)
    done
;;

(* CR avatar: undo *)
(*
      Lwt.async (fun () -> run_silent_drops t))
      *)

let rec sound_rain t id =
  match Hashtbl.find t.sound_rain_ids id with
  | None ->
    if Hashtbl.length t.sound_rains > t.num_sources
    then None
    else (
      let taken_rains = Hashtbl.data t.sound_rain_ids in
      let is_free id =
        not (List.mem taken_rains id ~equal:Rain.Id.equal)
      in
      let free_rains =
        Hashtbl.keys t.sound_rains |> List.filter ~f:is_free
      in
      let rain_id =
        match free_rains with
        | [] -> Rain.Id.create ()
        | free_rains -> List.random_element_exn free_rains
      in
      let rain = find_or_add_rain t `sound ~id:rain_id in
      Hashtbl.add_exn t.sound_rain_ids ~key:id ~data:rain_id;
      Some rain)
  | Some rain_id ->
    (match Hashtbl.find t.sound_rains rain_id with
     | Some rain -> Some rain
     | None ->
       Hashtbl.remove t.sound_rain_ids id;
       sound_rain t id)
;;

let delete_sound_rain t id =
  match Hashtbl.find t.sound_rain_ids id with
  | None -> ()
  | Some rain_id ->
    (* CR-someday: this logic trips you up. Maybe redefine saturation for
           different rains? *)
    let saturation_threshold =
      match t.config.color_flow with
      | Fade_to_base -> 0.4
      | Fade_to_none | Fade_to_none_smooth -> 0.
    in
    let remove_source () = Hashtbl.remove t.sound_rain_ids id in
    (match Hashtbl.find t.sound_rains rain_id with
     | None -> remove_source ()
     | Some rain ->
       if Float.(Rain.saturation rain >= saturation_threshold)
       then (
         remove_source ();
         Hashtbl.remove t.sound_rains rain_id))
;;

let rec update_num_sources_loop t =
  let%bind () = Lwt_js.sleep 30. in
  let shift = Random.int 3 - 1 in
  t.num_sources <- t.num_sources + shift;
  t.num_sources <- max t.num_sources 1;
  t.num_sources <- min t.num_sources t.config.max_sound_sources;
  let rec trim () =
    if Hashtbl.length t.sound_rains > t.num_sources
    then (
      let id, _ = Hashtbl.choose_exn t.sound_rain_ids in
      delete_sound_rain t id;
      trim ())
  in
  trim ();
  update_num_sources_loop t
;;

let set_config t config =
  t.config <- config;
  Hashtbl.iter t.elts ~f:(fun elt -> elt.config <- config);
  Option.iter t.listener ~f:(Sound.stop_listening t.sound);
  let listener =
    match config.on_sound with
    | None -> None
    | Some on_sound ->
      Sound.start t.sound;
      (match on_sound with
       | Wave { max_drops_per_second } ->
         Sound.on_wave t.sound ~f:(fun event ->
           match event with
           | Beat _ -> assert false
           | Wave intensity ->
             let open Float in
             let num_drops = intensity * max_drops_per_second in
             let num_drops =
               if num_drops > 1.
               then Int.of_float num_drops
               else if Random.float 1. < num_drops
               then 1
               else 0
             in
             for _ = 1 to num_drops do
               drop t
             done
           | Delete source ->
             let id = Sound.Source.id source in
             delete_sound_rain t id)
       | Beat on_beat ->
         Sound.on_beat
           t.sound
           ~max_sources:config.max_sound_sources
           ~f:(function
           | Wave _ -> assert false
           | Beat source ->
             if (not (human_playing t)) && t.config.bot_active
             then (
               match sound_rain t (Sound.Source.id source) with
               | None -> ()
               | Some rain ->
                 (match on_beat with
                  | Burst { drops_at_once } ->
                    Lwt.async (fun () -> Rain.burst rain ~drops_at_once)
                  | Drop num_drops ->
                    for _ = 1 to num_drops do
                      Rain.drop rain ~flash:true
                    done))
           | Delete source ->
             let id = Sound.Source.id source in
             delete_sound_rain t id))
      |> Some
  in
  t.listener <- listener
;;

let rec debug_loop t =
  let%bind () = Lwt_js.sleep 5. in
  (* t.silent_drop_count <- 0; *)
  debug [%message (t : t)];
  debug_loop t
;;

let () = ignore debug_loop

let start t =
  (* Lwt.async (fun () -> debug_loop t); *)
  Lwt.async (fun () -> update_num_sources_loop t);
  start_silent_rains t
;;

let create ~config ~sound elts =
  let t = create ~config ~sound elts in
  set_config t config;
  start t;
  t
;;

let human_touch t shape color =
  t.last_human_touch <- Time.now ();
  match Hashtbl.find t.elts (Shape.id shape) with
  | None -> ()
  | Some elt -> E.touch elt ~color ~flash:true
;;

let render t ~perspective ~pixi =
  Hashtbl.iter t.elts ~f:(E.render ~perspective ~pixi)
;;
