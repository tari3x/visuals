(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

let config : Grid_lib.Config.t =
  { drawing_mode = false
  ; debug_sound  = false
  ; screen_grid = false
  ; bot_active_at_start = true
  ; grid_kind = `free
  ; global_channel_name = "global-wall"
  ; color_flow = `fade_to_black
  }
