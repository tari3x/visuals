(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Float
open Common

type t =
  { mutable value : float
  ; mutable last_sample : Time.t
  ; decay : float
  } [@@deriving fields]

let add_sample t ~time ~value =
  let delta_t = Time.(time - t.last_sample |> Span.to_sec) in
  let alpha = exp (neg (t.decay * delta_t)) in
  let value = alpha * t.value + (1. - alpha) * value in
  t.value <- value;
  t.last_sample <- time

let create ~half_life ~time ~value =
  let decay = log 2. / Time.Span.to_sec half_life in
  { value
  ; last_sample = time
  ; decay
  }
