(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal
module C = Spark_lib.Config

let skin =
  { C.Skin.default with
    base_color = Color.white
  ; bot_active = true
  ; color_flow = Fade_to_none
  ; on_sound = Some Rain
  ; num_silent_rains = 0
  ; segment_life_span = Time.Span.of_sec 3.
  ; rain = { C.Rain.default with keep_raining_probability = 0.95 }
  ; max_sound_sources = 3
  }
;;

let config : C.t =
  { drawing_mode = false
  ; debug_sound = false
  ; calibration = Clicks
  ; global_channel_name = "global-wall"
  ; sparks = [ Free skin ]
  }
;;
