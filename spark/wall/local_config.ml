(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

module C = Spark_lib.Config

let config : C.t =
  { drawing_mode = false
  ; base_color = Color.white
  ; debug_sound  = false
  ; calibration = Clicks
  ; bot_active = true
  ; shapes = Free
  ; global_channel_name = "global-wall"
  ; color_flow = `fade_to_black
  ; num_sound_sources = 3
  ; on_sound = Some `rain
  ; num_silent_rains = 0
  ; rain =
      { C.Rain.default with
        keep_raining_probability = 0.95
      }
  ; segment_life_span = Time.Span.of_sec 3.
  }
