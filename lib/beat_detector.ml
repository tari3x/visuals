(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common_lib
open Common

let long_half_life = 10.
let short_half_life = 3.

let min_cutoff = 0.25

type t =
  { mutable value : float
  ; mutable beat_max : float option
  ; mutable total_max : float
  ; ewma : Ewma.t
  ; short_ewma_unused : Ewma.t
  }

let create () =
  let value = 0. in
  let ewma = Ewma.create ~half_life:long_half_life in
  let short_ewma_unused = Ewma.create ~half_life:short_half_life in
  { value
  ; beat_max = None
  ; total_max = 0.
  ; ewma
  ; short_ewma_unused
  }

let add_sample t ~time ~value =
  let open Float in
  Ewma.add_sample t.ewma       ~param:(Time.to_sec time) ~value;
  (* Ewma.add_sample t.short_ewma ~param:(Time.to_sec time) ~value; *)
  let beat_max =
    if value < min_cutoff * t.total_max
    then None
    else match t.beat_max with
    | None ->
      (* CR-someday: the lower end of the spectrum never hits 2. *)
      if value > 2. * Ewma.value_exn t.ewma
      then Some value
      else None
    | Some beat_max ->
      if value > beat_max
      then Some value
      else if value < 0.9 * beat_max
      then None
      else Some beat_max
  in
  t.value <- value;
  t.beat_max <- beat_max;
  t.total_max <- Float.max value t.total_max

let in_beat t =
  Option.is_some t.beat_max

module Debug = struct
  let ewma t =
    Ewma.value t.ewma

  let short_ewma t =
    Ewma.value t.short_ewma_unused
end
