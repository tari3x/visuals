(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common_lib
open Common

(* Effectively disable. I quite like the more active option for Fred. *)
let half_life = 10.
let pseudo_min_window = Time.Span.of_sec 0.5
let pseudo_min_ewma_half_life = 0.5
let min_cutoff = 0.25

type t =
  { mutable value : float
  ; mutable beat_max : float option
  ; mutable total_max : float
  ; ewma : Ewma.t
  ; (* remember the last minimum you saw, when it becomes too old, switch to
       current value *)
    mutable pseudo_min : float
  ; mutable pseudo_min_time : Time.t
  ; pseudo_min_ewma : Ewma.t
  ; min : Window_min.t
  }

let create () =
  let value = 0. in
  let ewma = Ewma.create ~half_life in
  let pseudo_min_ewma = Ewma.create ~half_life:pseudo_min_ewma_half_life in
  let min = Window_min.create ~window:1. in
  { value
  ; beat_max = None
  ; total_max = 0.
  ; ewma
  ; pseudo_min_ewma
  ; pseudo_min = 0.
  ; pseudo_min_time = Time.epoch
  ; min
  }
;;

let update_pseudo_min t ~value =
  let now_ = Time.now () in
  if Time.(t.pseudo_min_time + pseudo_min_window < now_)
     || Float.(value < t.pseudo_min)
  then (
    t.pseudo_min <- value;
    t.pseudo_min_time <- now_)
;;

let add_sample t ~time ~value =
  let open Float in
  update_pseudo_min t ~value;
  let param = Time.to_sec time in
  Ewma.add t.ewma ~param ~value;
  Ewma.add t.pseudo_min_ewma ~param ~value:t.pseudo_min;
  (* Ewma.add t.short_ewma ~param:(Time.to_sec time) ~value; *)
  let beat_max =
    if value < min_cutoff * t.total_max
    then None
    else (
      match t.beat_max with
      | None ->
        (* CR-someday: the lower end of the spectrum never hits 2. *)
        if value > 2. * Ewma.value_exn t.ewma then Some value else None
      | Some beat_max ->
        if value > beat_max
        then Some value
        else if value < 0.9 * beat_max
        then None
        else Some beat_max)
  in
  t.value <- value;
  t.beat_max <- beat_max;
  t.total_max <- Float.max value t.total_max
;;

let in_beat t = Option.is_some t.beat_max

module Debug = struct
  let ewma t = Ewma.value t.ewma
  let wave t = Ewma.value t.pseudo_min_ewma
  let pseudo_min t = t.pseudo_min
end
