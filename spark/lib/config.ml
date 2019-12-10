(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

module Calibration = struct
  type t =
  | Skip
  | Clicks
  | Laptop_aspect_ratio
      [@@deriving sexp]
end

module Rain = struct
  type t =
    { rain_dropoff : float
    ; wind_dropoff : float
    ; new_strand_probability : float
    ; fade_to_base_interpolation_arg : float
    ; drop_interval : Time.Span.t
    ; mutable keep_raining_probability : float
    } [@@deriving sexp]

  let default =
    { rain_dropoff = 0.5 (* 1.5 *)
    ; wind_dropoff = 10.
    (* CR-someday is 0.01 equivalent to infinity? *)
    ; new_strand_probability = 0.01
    (* Do not set lower than 1/256 since color is an int. *)
    ; fade_to_base_interpolation_arg = 0.01 (* 0.03 *)
    ; drop_interval = Time.Span.of_sec 0.01
    ; keep_raining_probability = 0.8
    }
end

module Skin = struct
  module Color_flow = struct
    type t =
    | Fade_to_black
    | Fade_to_black_smooth
    | Fade_to_base
        [@@deriving sexp]
  end

  type t =
    { base_color : Color.t
    ; flash_cutoff : float
    ; flash_top : float
    ; flash_color_weight : float
    ; flash_duration : float
    ; human_playing_timeout : Time.Span.t
    ; drops_period_range : Time.Span.t * Time.Span.t
    ; drops_value_range : float * float
    ; segment_life_span : Time.Span.t
    ; color_flow : Color_flow.t
    ; rain : Rain.t
    ; num_silent_rains : int
    ; on_sound : [ `rain | `drop of int ] option
    ; mutable bot_active : bool
    } [@@deriving sexp, fields]

  let default =
    { base_color = Color.white
    ; flash_cutoff = 0.4
    ; flash_top = 0.6
    ; flash_color_weight = 0.2 (* 0.15 *)
    ; flash_duration = 0.1
    ; human_playing_timeout = Time.Span.of_sec 10.
    ; drops_period_range = (Time.Span.of_sec 2., Time.Span.of_sec 10.)
    ; drops_value_range = (0.1, 1.) (*  (0.0000002, 0.3) *)
    ; segment_life_span = Time.Span.of_sec 3.
    ; color_flow = Fade_to_black
    ; rain = Rain.default
    ; num_silent_rains = 0
    ; on_sound = Some `rain
    ; bot_active = true
    }

  let validate_no_sound t =
    assert (Option.is_none t.on_sound)
end

module Sparks = struct
  type t =
  | Grid of { skin : Skin.t; rows : int; cols : int }
  | Hex of
      { tile_skin : Skin.t
      ; wire_skin : Skin.t
      }
  | Free of Skin.t
      [@@deriving sexp]

  let skins = function
    | Grid { skin; _ } -> [ skin ]
    | Free skin -> [ skin ]
    | Hex { tile_skin; wire_skin } -> [ tile_skin; wire_skin ]

  let validate_no_sound t =
    skins t |> List.iter ~f:Skin.validate_no_sound
end

type t =
  { drawing_mode : bool
  ; debug_sound : bool
  ; calibration : Calibration.t
  ; sparks : Sparks.t
  ; global_channel_name : string
  ; num_sound_sources : int
  } [@@deriving sexp]

let validate t =
  if t.num_sound_sources = 0
  then Sparks.validate_no_sound t.sparks

(* CR-someday: do we even use this? *)
let t_of_sexp s =
  let t = t_of_sexp s in
  validate t;
  t

let max_box_age t : Time.Span.t =
  if t.drawing_mode
  then Time.Span.of_sec Float.infty
  else Time.Span.of_sec 30.
