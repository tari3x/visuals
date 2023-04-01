(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal
module C = Spark_lib.Config

let skin =
  { C.Skin.default with
    base_color = Color.black
  ; bot_active = true
  ; color_flow = Fade_to_base
  ; on_sound = Some (Beat (Drop 3))
  ; num_silent_rains = 0
  ; rain = C.Rain.default
  ; segment_life_span = Time.Span.of_sec 3.
  ; max_sound_sources = 4 (* 2 *)
  }
;;

let shapes = Load_shapes.load ()

let config : C.t =
  { drawing_mode = false
  ; debug_sound = false
  ; calibration = Aspect_ratio { x = 1707.; y = 1133. }
  ; global_channel_name = "global-fred"
  ; sparks = [ Free { skin; shapes } ]
  }
;;
