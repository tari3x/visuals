(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Float

type t =
  { mutable value : float option
  ; mutable last_sample_param : float
  ; decay : float
  } [@@deriving fields]

let add_sample t ~param ~value =
  t.last_sample_param <- param;
  match t.value with
  | None -> t.value <- Some value
  | Some prev_value ->
    let delta_t = param - t.last_sample_param in
    let alpha = exp (neg (t.decay * delta_t)) in
    let value = alpha * prev_value + (1. - alpha) * value in
    t.value <- Some value

let create ~half_life =
  let decay = log 2. / half_life in
  { value = None
  ; last_sample_param = 0.
  ; decay
  }

let value_exn t =
  value t |> Option.value_exn
