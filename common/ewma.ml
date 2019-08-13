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
  } [@@deriving sexp, fields]

let add_sample t ~param ~value =
  let value =
    match t.value with
    | None -> value
    | Some prev_value ->
      let delta_t = param - t.last_sample_param in
      let alpha = exp (neg (t.decay * delta_t)) in
      alpha * prev_value + (1. - alpha) * value
  in
  t.last_sample_param <- param;
  t.value <- Some value

let create ~half_life =
  let decay = log 2. / half_life in
  { value = None
  ; last_sample_param = 0.
  ; decay
  }

let value_exn t =
  value t |> Option.value_exn

let%expect_test _ =
  let t = create ~half_life:1. in
  let print () =
    Caml.Printf.printf !"%{Sexp}" [%message (t : t)]
  in
  let add_sample param value =
    add_sample t ~param ~value;
    print ()
  in
  print ();
  [%expect {| (t((value())(last_sample_param 0)(decay 0.69314718055994529))) |}];
  add_sample 1. 1.;
  [%expect {| (t((value(1))(last_sample_param 1)(decay 0.69314718055994529))) |}];
  add_sample 2. 1.;
  [%expect {| (t((value(1))(last_sample_param 2)(decay 0.69314718055994529))) |}];
  add_sample 3. 4.;
  [%expect {| (t((value(2.5))(last_sample_param 3)(decay 0.69314718055994529))) |}]
