(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

(* TODO:

   * borders?
   * measure how the average brightness develops
   * resonant flow
   * solve slow movement together with color variety
   * make rains avoid each other
   * make rains die at different times
   * why the fuck does it fade out with time?

*)

module CF = Color_flow
module PD = Probability_distribution

let flash_cutoff = 0.94 (* 0.7 *)
let human_playing_timeout = Time.Span.of_sec 10.
let max_silent_rain_age = Time.Span.of_sec 20.
let rain_interval = Time.Span.of_sec 1.5
let drop_interval = Time.Span.of_sec 0.1
let silent_base_interpolation_arg = 0.4
let fade_to_base_interpolation_arg = 0.1
let max_active_rains = 1
let num_silent_rains = 3

module type Elt = sig
  module Id : Id

  type t

  val id : t -> Id.t
  val distance : t -> t -> float
  val touch : t -> Color_flow.t -> unit
end

module Make(Elt : Elt) = struct
  module E = struct
    module Id = Elt.Id

    type t =
      { config : Config.t
      ; elt : Elt.t
      ; mutable base_color : Color.t
      } [@@deriving fields]

    let distance t1 t2 =
      Elt.distance t1.elt t2.elt

    let fade ~config ~base_color ~flash =
      let open Time.Span in
      let base_color alpha = Color.set_alpha base_color ~alpha in
      let sls = Config.segment_life_span config in
      let final_alpha =
        match config.color_flow with
        | `fade_to_black -> 0.
        | `fade_to_base -> flash_cutoff
      in
      if not flash
      then begin
        CF.start_now (base_color flash_cutoff)
        |> CF.add ~after:sls ~color:(base_color final_alpha)
      end
      else begin
        CF.start_now (base_color 1.)
        |> CF.add ~after:(sls * 0.1) ~color:(base_color flash_cutoff)
        |> CF.add ~after:(sls * 0.9) ~color:(base_color final_alpha)
      end

    let create elt ~(config : Config.t) =
      let base_color = config.base_color in
      let color = fade ~config ~base_color ~flash:false in
      Elt.touch elt color;
      { base_color; elt; config }

    let touch t ~color ~flash =
      (* CR-someday: if we are faded out, why interpolate? *)
      let base_color =
        match t.config.color_flow with
        | `fade_to_black ->
          Color.interpolate [t.base_color; color] ~arg:0.5
        | `fade_to_base ->
          Color.interpolate [t.base_color; color]
            ~arg:fade_to_base_interpolation_arg
      in
      t.base_color <- base_color;
      let color = fade ~config:t.config ~base_color ~flash in
      Elt.touch t.elt color
  end

  module Rain = struct
    module Id = struct
      module Silent = Id(struct let name = "Rain" end)

      type t =
      | Sound of Sound.Source.Id.t
      | Silent of Silent.t
          [@@deriving hash, compare]

      let equal = [%compare.equal : t]

      let create_silent () =
        Silent (Silent.create ())
    end

    type t =
      { id : Id.t
      ; config : Config.t
      ; elts : E.t PD.t option
      ; color : Color.t
      ; mutable active : bool
      } [@@deriving fields]

    let create ~elts ~(id : Id.t) ~config =
      let open Float in
      let centre = List.random_element_exn elts in
      (* let color = Color.random () |> Color.maximize in *)
      let color =
        match id with
        | Sound _ -> Color.random ()
        | Silent _ ->
          Color.interpolate [E.base_color centre; Color.random ()]
            ~arg:silent_base_interpolation_arg
      in
      let color = Color.maximize color in
      let elts =
        match elts with
        | [] -> None
        | elts ->
          let max_weight, other_elts =
            List.filter elts ~f:(fun e -> not (phys_equal e centre))
            |> List.fold_map ~init:0. ~f:(fun max_weight e ->
              let distance = E.distance centre e in
              let weight = int_pow (1. / distance) 3 in
              Float.max max_weight weight, (e, weight))
          in
          (*
            let other_elts =
            List.map other_elts ~f:(fun (segment, weight) ->
            let weight =
            if weight < max_weight / 2. then 0. else weight
            in
            (segment, weight))
            in
          *)
          PD.create_exn
            (( centre, max_weight ) :: other_elts)
          |> Some
      in
      (* debug !"elts: %{sexp: Elt.t PD.t}" elts; *)
      { id; config; color; elts
      ; active = false
      }

    let burst t =
      let open Lwt.Let_syntax in
      match t.elts with
      | None -> Lwt.return ()
      | Some elts ->
        let rec loop ~is_first =
          let open Float in
          let stop () =
            t.active <- false;
            Lwt.return ()
          in
          let%bind () = Lwt_js.sleep (Time.Span.to_sec drop_interval) in
          if Random.float 1. > t.config.keep_raining_probability
          then stop ()
          else begin
            let elt = PD.draw elts in
            let color = t.color in
            let flash =
              match t.id with
              | Sound _ -> is_first
              | Silent _ -> true
            in
            E.touch elt ~color ~flash;
            loop ~is_first:false
          end
        in
        t.active <- true;
        loop ~is_first:true
  end

  module Rain_table = Ephemeron.Make(Rain.Id)

  type t =
    { config : Config.t
    ; elts : E.t Hashtbl.M(E.Id).t (* not empty *)
    ; sound : Sound.t
    ; rains : Rain.t Rain_table.t
    ; mutable last_human_touch : Time.t
    }

  let set_elts t elts =
    Hashtbl.clear t.elts;
    List.iter elts ~f:(fun elt ->
      let key = Elt.id elt in
      let data = E.create elt ~config:t.config in
      Hashtbl.set t.elts ~key ~data)

  let create ~(config : Config.t) ~sound elts =
    let t =
      { config; sound
      ; rains = Rain_table.create ()
      ; elts = Hashtbl.create (module E.Id) ()
      ; last_human_touch = Time.(sub (now ()) human_playing_timeout)
      }
    in
    set_elts t elts;
    t

  let human_playing t =
    let open Time in
    let open Span in
    now () - t.last_human_touch < human_playing_timeout

  let new_rain t id =
    Rain.create ~id ~elts:(Hashtbl.data t.elts) ~config:t.config

  let find_or_add_rain t ~id =
    Rain_table.find_or_add t.rains id ~default:(fun () -> new_rain t id)

  let num_active_rains t =
    Rain_table.count t.rains ~f:Rain.active

  let run_silent_rain t =
    let open Lwt.Let_syntax in
    let rec loop id start_time =
      let too_old =
        let open Time in
        let open Span in
        now () - start_time > max_silent_rain_age
      in
      let%bind () = Lwt_js.sleep (Time.Span.to_sec rain_interval) in
      if too_old then start_new ()
      else if
          num_active_rains t >= max_active_rains
          || Float.(Random.float 1. > t.config.start_silent_rain_probability)
      then loop id start_time
      else begin
        let rain = find_or_add_rain t ~id in
        let%bind () = Rain.burst rain in
        loop id start_time
      end
    and start_new () =
      let id = Rain.Id.create_silent () in
      let start_time = Time.now () in
      loop id start_time
    in
    start_new ()

  let start_silent_rains t =
    for _ = 1 to num_silent_rains do
      Lwt.async (fun () -> run_silent_rain t)
    done

  let start t =
    start_silent_rains t;
    if t.config.start_rain_on_sound
    then begin
      Sound.start t.sound;
      Sound.on_beat t.sound ~f:(fun source ->
        if not (human_playing t) && t.config.bot_active
        then begin
          let id = Rain.Id.Sound (Sound.Source.id source) in
          let rain = find_or_add_rain t ~id in
          Lwt.async (fun () -> Rain.burst rain)
        end)
    end

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

