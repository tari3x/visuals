open Std_internal

type t =
  { drawing_mode : bool
  ; debug_sound : bool
  ; screen_grid : bool
  ; bot_active_at_start : bool
  ; grid_kind : [ `grid | `free ]
  ; global_channel_name : string
  }

let max_box_age t : Time.Span.t =
  if t.drawing_mode
  then Time.Span.of_sec Float.infty
  else Time.Span.of_sec 30.

let start_color t =
  if t.drawing_mode
  then Color.black
  else Color.white

let segment_life_span t =
  if t.drawing_mode
  then Float.infty
  else 3.
