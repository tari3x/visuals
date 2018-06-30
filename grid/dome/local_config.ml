(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

module C = Grid_lib.Config

let config : C.t =
  { drawing_mode = false
  ; debug_sound  = false
  ; skip_calibration = true
  ; bot_active = true
  ; grid_kind = `free
  ; global_channel_name = "global-wall"
  ; color_flow = `fade_to_base
  ; start_rain = `with_probability 0.9
  ; keep_raining_probability = C.keep_raining_probability
  }