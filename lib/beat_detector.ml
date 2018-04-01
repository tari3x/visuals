(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common

let half_life =
  Time.Span.of_sec 10.

type t =
  { mutable value : float
  ; mutable beat_max : float option
  ; ewma : Ewma.t
  }

let create ~time =
  let value = 0. in
  let ewma = Ewma.create ~half_life ~time ~value in
  { value
  ; beat_max = None
  ; ewma
  }

let add_sample t ~time ~value =
  let open Float in
  Ewma.add_sample t.ewma ~time ~value;
  let beat_max =
    match t.beat_max with
    | None ->
      if value > 2. * Ewma.value t.ewma
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
  t.beat_max <- beat_max

let in_beat t =
  Option.is_some t.beat_max

module Debug = struct
  let ewma t =
    Ewma.value t.ewma
end
