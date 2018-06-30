(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Std_internal

module CF = Color_flow
module PD = Probability_distribution

let flash_cutoff = 0.4 (* 0.7 *)
let human_playing_timeout = Time.Span.of_sec 10.

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
      }

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

    let create elt ~config =
      let base_color = Config.base_color config in
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
          Color.interpolate [t.base_color; color] ~arg:0.3
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
      ; elts : E.t PD.t
      ; color : Color.t
      }

    let create ~elts ~id ~config =
      let open Float in
      let color = Color.random () |> Color.maximize in
      let centre = List.random_element_exn elts in
      let elts =
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
      in
      (* debug !"elts: %{sexp: Elt.t PD.t}" elts; *)
      { id; config; color; elts }

    let burst t =
      let rec loop ~is_first =
        let open Float in
        let elt = PD.draw t.elts in
        let color = t.color in
        (* interpolation in touch should already create enough variation. *)
        (*
          let color =
          Color.interpolate [ base_color; Color.random () ] ~arg:0.1
          in
        *)
        E.touch elt ~color ~flash:is_first;
        if Random.float 1. > t.config.keep_raining_probability
        then Lwt.return ()
        else begin
          Lwt_js.sleep 0.05
          >>= fun () ->
          loop ~is_first:false
        end
      in
      Lwt.async (fun () -> loop ~is_first:true)
  end

  module Rain_table = Ephemeron.Make(Rain.Id)

  type t =
    { config : Config.t
    ; sound : Sound.t
    ; rains : Rain.t Rain_table.t
    ; elts : E.t Hashtbl.M(E.Id).t
    ; mutable last_human_touch : Time.t
    }

  let create ~(config : Config.t) ~sound =
    { config; sound
    ; rains = Rain_table.create ()
    ; elts = Hashtbl.create (module E.Id) ()
    ; last_human_touch = Time.(sub (now ()) human_playing_timeout)
    }

  let set_elts t elts =
    Hashtbl.clear t.elts;
    List.iter elts ~f:(fun elt ->
      let key = Elt.id elt in
      let data = E.create elt ~config:t.config in
      Hashtbl.set t.elts ~key ~data)

  let human_playing t =
    let open Time in
    let open Span in
    now () - t.last_human_touch < human_playing_timeout

  let new_rain t id =
    Rain.create ~id ~elts:(Hashtbl.data t.elts) ~config:t.config

  let find_or_add_rain t ~id =
    Rain_table.find_or_add t.rains id ~default:(fun () -> new_rain t id)

  let has_elements t =
    not (Hashtbl.is_empty t.elts)

  let run t =
    let open Float in
    match t.config.start_rain with
    | `on_sound_source ->
      Sound.on_beat t.sound ~f:(fun source ->
        if not (human_playing t) && t.config.bot_active && has_elements t
        then begin
          let id = Rain.Id.Sound (Sound.Source.id source) in
          let rain = find_or_add_rain t ~id in
          Rain.burst rain
        end);
      Sound.start t.sound
    | `with_probability p ->
      Lwt.every (Time.Span.of_sec 1.) ~f:(fun () ->
        if Random.float 1. < p && has_elements t
        then begin
          let id = Rain.Id.create_silent () in
          new_rain t id |> Rain.burst
        end)

  let start ~config ~sound =
    let t = create ~config ~sound in
    run t;
    t

  let human_touch t elt color =
    t.last_human_touch <- Time.now ();
    match Hashtbl.find t.elts (Elt.id elt) with
    | None -> ()
    | Some elt ->
      E.touch elt ~color ~flash:true
end

