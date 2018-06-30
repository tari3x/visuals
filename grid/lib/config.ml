(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

let keep_raining_probability = 0.95

type t =
  { drawing_mode : bool
  ; debug_sound : bool
  ; skip_calibration : bool
  ; mutable bot_active : bool
  ; grid_kind : [ `grid | `free ]
  ; global_channel_name : string
  ; color_flow : [ `fade_to_black | `fade_to_base ]
  ; start_rain : [ `on_sound_source | `with_probability of float ]
  ; mutable keep_raining_probability : float
  } [@@deriving sexp]

let max_box_age t : Time.Span.t =
  if t.drawing_mode
  then Time.Span.of_sec Float.infty
  else Time.Span.of_sec 30.

let base_color t =
  if t.drawing_mode
  then Color.black
  else Color.white

let segment_life_span t =
  if t.drawing_mode
  then Time.Span.of_sec Float.infty
  else Time.Span.of_sec 3.
