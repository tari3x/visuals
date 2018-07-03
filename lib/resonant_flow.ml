(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common

module Period = struct
  type t =
    { span : Time.Span.t
    ; start : Time.t
    (* Minux x coordinate of observation point, between 0 and 1. This is chosen
       such that we switch a new period when value is [max_value] and the
       function remains continuous. *)
    ; d : float
    ; value_mult : float
    }

  let create
      ~period_range:(min_period, max_period)
      ~value_range:(min_v, max_v)
      =
    let open Float in
    let min_period = Time.Span.to_sec min_period in
    let max_period = Time.Span.to_sec max_period in
    let span =
      min_period + Random.float (max_period - min_period)
      |> Time.Span.of_sec
    in
    let start = Time.now () in
    let max_d =
      let v = min_v / max_v in
      (1. - v) / (1. + v)
    in
    (* let d = Random.float max_d in *)
    let d = max_d in
    let value_mult = max_v / (1. + d) in
    { span; start; d; value_mult }

  let elapsed t =
    Time.(now () > t.start + t.span)

  let eval t =
    let open Float in
    let phase =
      2. * pi * (Time.Span.to_sec Time.(now () - t.start))
      / Time.Span.to_sec t.span
    in
    let x = cos phase in
    let y = sin phase in
    let result =
      t.value_mult * sqrt (int_pow y 2 + int_pow (x - t.d) 2)
    in
    (* debug "%f" result; *)
    result
end

type t =
  { period_range : Time.Span.t * Time.Span.t
  ; value_range  : float * float
  ; mutable period : Period.t
  }

let check_interval
    (type t)
    (module V: Comparable_and_to_stringable with type t = t)
    (v1, v2) ~name =
  if V.(v1 > v2)
  then failwithf !"%s range not ordered: %{V}, %{V}" name v1 v2 ()

let create_exn ~period_range ~value_range =
  check_interval (module Time.Span) period_range ~name:"period";
  check_interval (module Float)     value_range  ~name:"value";
  let period = Period.create ~period_range ~value_range in
  { period_range
  ; value_range
  ; period
  }

let eval t =
  let period_range = t.period_range in
  let value_range  = t.value_range in
  if Period.elapsed t.period
  then t.period <- Period.create ~period_range ~value_range;
  Period.eval t.period
