(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

module C = Grid_lib.Config

let config : C.t =
  { drawing_mode = false
  ; base_color = Color.black
  ; debug_sound  = false
  ; skip_calibration = true
  ; bot_active = true
  ; grid_kind = `free
  ; global_channel_name = "global-wall"
  ; color_flow = `fade_to_base
  ; start_rain_on_sound = false
  ; start_silent_rain_probability = 0.9
  ; keep_raining_probability = 0.9
  }
