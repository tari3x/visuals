(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal
module C = Spark_lib.Config

let wire_intensity = 6.
let tile_intensity = 20.
let zoom = 1.
let new_strand_probability = 0.05

let hex ~wire_flash_mult ~tile_flash_mult ~r1_mult ~tiles =
  let open Float in
  let rain = { Config.Rain.default with wind_dropoff = 1.5 } in
  let tile_rain =
    {
      rain with
      new_strand_probability = tile_intensity * new_strand_probability;
      keep_raining_probability = 0.95;
    }
  in
  (* This one doesn't matter, right, cause it's rain? *)
  let wire_rain =
    {
      rain with
      keep_raining_probability = 0.9 (* 0.6 *);
      drop_interval = Time.Span.of_sec 0.01;
      rain_dropoff = 1.;
      new_strand_probability;
    }
  in
  let tile_skin =
    {
      Config.Skin.default with
      rain = tile_rain;
      segment_life_span = Time.Span.(of_sec 2.);
      flash_top = 0.5 * tile_flash_mult;
      flash_cutoff = 0.3 * tile_flash_mult;
      flash_duration = 0.5;
      color_flow = Fade_to_none;
      max_sound_sources = 2;
      base_color = Color.none;
    }
  in
  let wire_skin =
    {
      Config.Skin.default with
      rain = wire_rain;
      segment_life_span = Time.Span.(of_sec 10.);
      flash_top = 1. * wire_flash_mult;
      flash_cutoff = 0.35 * wire_flash_mult;
      flash_duration = 0.5;
      color_flow = Fade_to_none_smooth;
      on_sound =
        Some
          (Wave
             {
               max_drops_per_second = wire_intensity * 100.;
               flash_probability = 0.4;
             });
      max_sound_sources = 2;
      base_color = Color.none;
    }
  in
  let tile_skin = if tiles then Some tile_skin else None in
  Config.Sparks.Hex { tile_skin; wire_skin; r1_mult }

let config : C.t =
  let open Float in
  {
    drawing_mode = false;
    debug_sound = false;
    calibration = Skip;
    sparks =
      [
        hex ~r1_mult:(zoom * 0.02) ~wire_flash_mult:1. ~tile_flash_mult:0.5
          ~tiles:true;
        hex ~r1_mult:(zoom * 0.04) ~wire_flash_mult:0.1 ~tile_flash_mult:0.
          ~tiles:true;
        hex ~r1_mult:(zoom * 0.06) ~wire_flash_mult:0.1 ~tile_flash_mult:0.
          ~tiles:false;
        hex ~r1_mult:(zoom * 0.08) ~wire_flash_mult:0.1 ~tile_flash_mult:0.4
          ~tiles:true;
      ];
    global_channel_name = "global-hex";
  }
