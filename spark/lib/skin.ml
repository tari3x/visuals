(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Js_of_ocaml_lwt
open Std_internal

(* TODO:

   * borders?
   * try tweaking centre max weight
   * try using manhattan distance or similar

   * try with chrome

   * smaller triangles

   * detangle continuing rain from beats, stop the rain at th border,
   for instance.

   * less random white

   * fewer colors

   * why degrades to dark with low fade_to_base_interpolation_arg? Is it
     something about alpha?

   * 3d elements, like actual cubes with sparks flowing over.

   * Check why drawing on 2D canvas so CPU intensive.
   Set alpha: false in [getContext] to try improving performance.

   * Why does fred seem to flash so much more than the sound debug?

   * Change long half life for the purpose of beat detection

   * latency
     https://askubuntu.com/questions/491825/pulseaudio-loopback-latency
     http://juho.tykkala.fi/Pulseaudio-and-latency
*)

module CF = Color_flow
module PD = Probability_distribution
module V = Vector

let flash_cutoff = 0.95
let human_playing_timeout = Time.Span.of_sec 10.

let rain_dropoff = 0.5 (* 1.5 *)
let wind_dropoff = 10.
let drop_interval = Time.Span.of_sec 0.01
let fade_to_base_interpolation_arg = 0.01 (* 0.03 *)
let drops_period_range = (Time.Span.of_sec 2., Time.Span.of_sec 10.)
let drops_value_range = (0.1, 1.) (*  (0.0000002, 0.3) *)
(* CR-someday is this equivalent to infinity? *)
let new_strand_probability = 0.01

module type Elt = sig
  module Id : Id

  type t

  val id : t -> Id.t
  val offset : t -> t -> V.t
  val touch : t -> Color_flow.t -> unit
end

module Make(Elt : Elt) = struct
  module Elts = struct
    type t = Elt.t list

    let create_exn elts : t =
      match elts with
      | [] | [_] -> failwith "Elts.create_exn: too short"
      | elts -> elts
  end

  module E = struct
    module Id = Elt.Id

    type t =
      { config : Config.t
      ; elt : Elt.t
      ; mutable base_color : Color.t
      } [@@deriving fields]

    let id t =
      Elt.id t.elt

    let compare_ids t1 t2 =
      Id.compare (id t1) (id t2)

    let offset t1 t2 =
      Elt.offset t1.elt t2.elt

    let distance t1 t2 =
      V.length (offset t1 t2)

    let a c alpha = Color.set_alpha c ~alpha

    let fade_to_black ~config ~base_color ~flash =
      let open Time.Span in
      let a c alpha = Color.set_alpha c ~alpha in
      let sls = Config.segment_life_span config in
      if not flash
      then begin
        CF.start_now (a base_color flash_cutoff)
        |> CF.add ~after:sls ~color:(a base_color 0.)
      end
      else begin
        CF.start_now (a base_color 1.)
        |> CF.add ~after:(sls * 0.1) ~color:(a base_color flash_cutoff)
        |> CF.add ~after:(sls * 0.9) ~color:(a base_color 0.)
      end

    (* Always flashes *)
    let fade_to_base ~config ~flash_color ~new_base =
      let open Time.Span in
      let sls = Config.segment_life_span config in
      CF.start_now (a (Color.maximize flash_color) 1.)
      |> CF.add ~after:(sls * 0.1) ~color:(a new_base flash_cutoff)

    let touch t ~color ~flash =
      (* CR-someday: if we are faded out, why interpolate? *)
      let base_color, color =
        match t.config.color_flow with
        | `fade_to_black ->
          let base_color = Color.interpolate [t.base_color; color] ~arg:0.5 in
          let color = fade_to_black ~config:t.config ~base_color ~flash in
          base_color, color
        | `fade_to_base ->
          let new_base =
            Color.interpolate [t.base_color; color]
              ~arg:fade_to_base_interpolation_arg
          in
          let flash_color =
            (* t.base_color *)
            Color.interpolate [t.base_color; color] ~arg:0.15
          in
          let color =
            fade_to_base ~config:t.config ~flash_color ~new_base
          in
          new_base, color
      in
      t.base_color <- base_color;
      Elt.touch t.elt color

    let create elt ~(config : Config.t) =
      let base_color = config.base_color in
      let t = { base_color; elt; config } in
      touch t ~color:base_color ~flash:false;
      t
  end

  module Rain = struct
    module Id = struct
      module Silent = Id(struct let name = "Rain" end)

      type t =
      | Sound of Sound.Source.Id.t
      | Silent of Silent.t
          [@@deriving hash, compare, sexp]

      let create_silent () =
        Silent (Silent.create ())
    end

    type t =
      { id : Id.t
      ; config : Config.t
      ; color : Color.t
      ; elts : E.t list
      ; centre : E.t
      ; wind : V.t
      ; next_strand : E.t PD.t
      ; mutable last_drop : E.t
      ; mutable centre_drops : int
      ; min_distance : float
      } [@@deriving fields]

    let choose_new_centre_exn ~elts ~other_rains =
      let cs =
        List.map other_rains ~f:centre
        |> List.dedup_and_sort ~compare:E.compare_ids
      in
      let is_degenerate =
        List.is_empty cs || List.length cs >= List.length elts
      in
      let weight e =
        if is_degenerate then 1.
        else if List.mem cs e ~equal:phys_equal then 0.
        else
          List.map cs ~f:(E.distance e)
          |> List.reduce_exn ~f:Float.(+)
      in
      List.map elts ~f:(fun e -> PD.Elt.create e ~weight:(weight e))
      |> PD.create_exn
      |> PD.draw

    let random_drop_around_centre ~min_distance centre elts ~dropoff =
      let open Float in
      let other_elts =
        List.filter elts ~f:(fun e -> not (phys_equal e centre))
        |> List.map ~f:(fun e ->
          let distance = E.distance centre e in
          let weight = (min_distance / distance) **. dropoff in
          PD.Elt.create e ~weight)
      in
      PD.create_exn (PD.Elt.create centre ~weight:1. :: other_elts)

    let create_exn ~other_rains ~min_distance ~elts ~(id : Id.t) ~config =
      let color = Color.random () |> Color.maximize in
      let centre = choose_new_centre_exn ~elts ~other_rains in
      let wind = V.(scale (random_unit ()) ~by:min_distance) in
      debug [%message (min_distance : float)];
      debug [%message (wind : V.t)];
      let next_strand =
        random_drop_around_centre
          centre
          elts
          ~min_distance
          ~dropoff:rain_dropoff
      in
      { id; config; color
      ; elts
      ; next_strand
      ; last_drop = centre
      ; centre
      ; wind
      ; min_distance
      ; centre_drops = 0
      }

    let next_strand_drop t =
      let open Float in
      let last_drop = t.last_drop in
      let weight e =
        let v = E.offset e last_drop in
        let d = Float.max 1. V.(length (v - t.wind)) in
        (1. /  d) **. wind_dropoff
      in
      let elts =
        List.map t.elts ~f:(fun e ->
          let weight = weight e in
          PD.Elt.create e ~weight)
      in
      let max_weight =
        List.map elts ~f:PD.Elt.weight
        |> List.max_elt ~compare:Float.compare
        |> Option.value_exn
      in
      if Float.(max_weight = weight last_drop)
      (* We hit the wall, start a new strand *)
      then PD.draw t.next_strand
      else PD.draw (PD.create_exn elts)

    let drop t ~flash =
      let e =
        let open Float in
        if Random.float 1. <= new_strand_probability
        then PD.draw t.next_strand
        else next_strand_drop t
      in
      t.last_drop <- e;
      if phys_equal e t.centre then t.centre_drops <- t.centre_drops + 1;
      E.touch e ~color:t.color ~flash

    (* This should be roughly enough to saturate the color *)
    let finished t =
      t.centre_drops >= int (4. /. fade_to_base_interpolation_arg)

    let is_silent t =
      match t.id with
      | Sound _ -> false
      | Silent _ -> true

    let burst t =
      let open Lwt.Let_syntax in
      let rec loop ~is_first =
        let open Float in
        let stop () = Lwt.return () in
        let%bind () = Lwt_js.sleep (Time.Span.to_sec drop_interval) in
        let flash =
          match t.id with
          | Sound _ -> is_first
          | Silent _ -> true
        in
        drop t ~flash;
        if Random.float 1. > t.config.keep_raining_probability
        then stop ()
        else loop ~is_first:false
      in
      loop ~is_first:true
  end

  type t =
    { config : Config.t
    ; elts : E.t Hashtbl.M(E.Id).t (* not empty *)
    ; sound : Sound.t
    ; rains : Rain.t Hashtbl.M(Rain.Id).t
    ; mutable min_distance : float
    ; mutable last_human_touch : Time.t
    ; mutable drop_count : int
    }

  let set_elts t (elts : Elts.t) =
    Hashtbl.clear t.elts;
    let min_distance =
      List.cartesian_product elts elts
      |> List.filter_map ~f:(fun (e1, e2) ->
        if phys_equal e1 e2 then None
        else Some (V.length (Elt.offset e1 e2)))
      |> List.min_elt ~compare:Float.compare
      |> Option.value_exn
    in
    t.min_distance <- min_distance;
    List.iter elts ~f:(fun elt ->
      let key = Elt.id elt in
      let data = E.create elt ~config:t.config in
      Hashtbl.set t.elts ~key ~data)

  let create ~(config : Config.t) ~sound elts =
    let t =
      { config; sound
      ; rains = Hashtbl.create (module Rain.Id)
      ; elts  = Hashtbl.create (module E.Id)
      ; last_human_touch = Time.(sub (now ()) human_playing_timeout)
      ; drop_count = 0
      ; min_distance = 0.
      }
    in
    set_elts t elts;
    t

  let human_playing t =
    let open Time in
    let open Span in
    now () - t.last_human_touch < human_playing_timeout

  let new_rain t id =
    Rain.create_exn ~id ~config:t.config
      ~min_distance:t.min_distance
      ~other_rains:(Hashtbl.data t.rains)
      ~elts:(Hashtbl.data t.elts)

  let find_or_add_rain t ~id =
    Hashtbl.find_or_add t.rains id ~default:(fun () -> new_rain t id)

  let run_silent_rain t =
    let open Lwt.Let_syntax in
    let rec loop rain =
      let%bind () = Lwt_js.sleep 1. in
      if not (Rain.finished rain) then loop rain
      else begin
        Hashtbl.remove t.rains (Rain.id rain);
        start_new ()
      end
    and start_new () =
      let id = Rain.Id.create_silent () in
      loop (find_or_add_rain t ~id)
    in
    start_new ()

  let silent_rains t =
    Hashtbl.data t.rains
    |> List.filter ~f:Rain.is_silent

   let drop t =
     t.drop_count <- t.drop_count + 1;
     Option.iter
       (List.random_element (silent_rains t))
       ~f:(Rain.drop ~flash:true)

  let run_silent_drops t =
    let open Lwt.Let_syntax in
    let open Float in
    (* CR-someday: surely there's something smarter possible that doesn't wake
       up every quantum if not many drops are happening. *)
    (* CR-someday: shurely the probability needs to scale with [quantum]? *)
    let quantum = 0.1 (* 0.001 *) in
    let drops =
      Resonant_flow.create_exn
        ~period_range:drops_period_range
        ~value_range:drops_value_range
    in
    let rec count_drops () =
      let%bind () = Lwt_js.sleep 1. in
      t.drop_count <- 0;
      count_drops ()
    in
    let rec loop () =
      let%bind () = Lwt_js.sleep quantum in
      let drop_probability = Resonant_flow.eval drops in
      if Random.float 1. <= drop_probability then drop t;
      loop ()
    in
    Lwt.async (fun () -> count_drops ());
    loop ()

  let start_silent_rains t =
    for _ = 1 to t.config.num_silent_rains do
      Lwt.async (fun () -> run_silent_rain t)
    done;
    if t.config.num_silent_rains > 0
    then Lwt.async (fun () -> run_silent_drops t)

  let start t =
    start_silent_rains t;
    match t.config.on_sound with
    | None -> ()
    | Some on_sound ->
      Sound.start t.sound;
      match on_sound with
      | `rain ->
        Sound.on_event t.sound ~f:(function
        | Beat source ->
          if not (human_playing t) && t.config.bot_active
          then begin
            let id = Rain.Id.Sound (Sound.Source.id source) in
            let rain = find_or_add_rain t ~id in
            Lwt.async (fun () -> Rain.burst rain)
          end
        | Delete source ->
          let id = Rain.Id.Sound (Sound.Source.id source) in
          Hashtbl.remove t.rains id)
      | `drop num_drops ->
        Sound.on_event t.sound ~f:(function
        | Beat _ ->
          if not (human_playing t) && t.config.bot_active
          then for _ = 1 to num_drops do drop t done
        | Delete _ -> ())

  let start ~config ~sound elts =
    let t = create ~config ~sound elts in
    start t;
    t

  let human_touch t elt color =
    t.last_human_touch <- Time.now ();
    match Hashtbl.find t.elts (Elt.id elt) with
    | None -> ()
    | Some elt ->
      E.touch elt ~color ~flash:true
end

