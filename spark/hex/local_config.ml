(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal
module C = Spark_lib.Config

let rain =
  { Config.Rain.default with
    wind_dropoff = 1.5
  ; new_strand_probability = 0.05
  ; keep_raining_probability = 0.6
  }
;;

let tile_rain = { rain with keep_raining_probability = 0.8 }
let wire_rain = rain
let skin = { Config.Skin.default with color_flow = Fade_to_black_smooth }

let tile_skin =
  let open Float in
  let flash_mult = 0.3 in
  { skin with
    rain = tile_rain
  ; segment_life_span = Time.Span.(of_sec 2.)
  ; flash_top = 0.23 * flash_mult
  ; flash_cutoff = 0.15 * flash_mult
  ; flash_duration = 0.5
  ; color_flow = Fade_to_black
  }
;;

let wire_skin =
  { skin with
    rain = wire_rain
  ; segment_life_span = Time.Span.(of_sec 5.)
  ; flash_top = 1.
  ; flash_cutoff = 0.35
  ; flash_duration = 0.5
  ; on_sound =
      Some (Wave { max_drops_per_second = 200.; flash_probability = 0.4 })
  }
;;

let config : C.t =
  { drawing_mode = false
  ; debug_sound = false
  ; calibration = Skip
  ; sparks = Hex { tile_skin; wire_skin }
  ; global_channel_name = "global-hex"
  ; num_sound_sources = 4
  }
;;
