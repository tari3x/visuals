(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common_lib
open Common
module MA = Moving_average
module MM = Moving_min
module LM = Local_min

(* CR: silence should reset long *)
(* CR: cut off far frequencies *)
let normalizer_window = 60.
let wave_window = 0.25
let beat_window = 0.05
let _beat_min_window = 0.3

(* 0.25 *)
let min_cutoff = 0.0

module Detector = struct
  type t =
    { threshold : float
    ; cutoff : float
    ; mutable value : float
    ; mutable max : float option
    ; mutable total_max : float
    }

  let create ~threshold ~cutoff =
    { threshold; cutoff; max = None; total_max = 0.; value = 0. }
  ;;

  (* CR-someday: update the min_cutoff logic. *)
  let add t ~base ~signal =
    let open Float in
    let value = if base = 0. then signal else signal / base in
    let max =
      if signal < min_cutoff * t.total_max
      then None
      else (
        match t.max with
        | None -> if value > t.threshold then Some value else None
        | Some max ->
          if value > max
          then Some value
          else if value <= t.cutoff * max
          then None
          else Some max)
    in
    t.value <- value;
    t.max <- max;
    t.total_max <- Float.max signal t.total_max
  ;;

  let active t = Option.is_some t.max
end

(* CR-someday: converge of long to 1. is really slow this way. *)
module Normalizer = struct
  type t =
    { average : MA.t
    ; variance : MA.t
    }

  let create () =
    let variance = MA.create ~window:normalizer_window in
    let average = MA.create ~window:normalizer_window in
    { variance; average }
  ;;

  let add t ~param ~value =
    let open Float in
    MA.add t.average ~param ~value;
    MA.add t.variance ~param ~value:(abs (value - MA.get_exn t.average))
  ;;

  let normalize t value =
    let open Float in
    let average = MA.get_exn t.average in
    let variance = MA.get_exn t.variance in
    if variance = 0. then 1. else 1. + ((value - average) / variance)
  ;;
end

module D = Detector
module N = Normalizer

(* CR-someday: remove wave and stuff if it remains unused. *)
type t =
  { normalizer : N.t
  ; wave_avg : MA.t
  ; beat_avg : MA.t
  ; beat_min : LM.t
  ; beat : D.t
  }

let create () =
  let normalizer = N.create () in
  let wave_avg = MA.create ~window:wave_window in
  let beat_avg = MA.create ~window:beat_window in
  let beat_min = LM.create () in
  let beat = D.create ~threshold:1.5 ~cutoff:0.9 in
  { normalizer; beat; wave_avg; beat_avg; beat_min }
;;

let add t ~time ~value =
  let param = Time.to_sec time in
  N.add t.normalizer ~param ~value;
  let value = N.normalize t.normalizer value in
  MA.add t.beat_avg ~param ~value;
  let beat = MA.get_exn t.beat_avg in
  LM.add t.beat_min beat;
  let beat_min = LM.get t.beat_min in
  MA.add t.wave_avg ~param ~value:beat_min;
  let wave = MA.get_exn t.wave_avg in
  D.add t.beat ~base:wave ~signal:beat
;;

let in_beat t = D.active t.beat
let wave t = LM.get t.beat_min

module Debug = struct
  let beat t = MA.get t.beat_avg
  let beat_min t = Some (LM.get t.beat_min)
end
