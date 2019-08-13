(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

module C = Grid_lib.Config

let config : C.t =
  { drawing_mode = false
  ; base_color = Color.white
  ; debug_sound  = false
  ; skip_calibration = false
  ; bot_active = true
  ; grid_kind = `free
  ; global_channel_name = "global-wall"
  ; color_flow = `fade_to_black
  ; on_sound = Some `rain
  ; num_silent_rains = 0
  ; keep_raining_probability = C.keep_raining_probability
  }