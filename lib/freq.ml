(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base

type t =
  { hertz : float
  ; mel : float
  } [@@deriving fields]

(* CR-someday: memoize *)
(* https://en.wikipedia.org/wiki/Mel_scale *)
let to_mel ~hertz =
  let open Float in
  (* Memo.general (fun h -> *)
  2595. * log10 (1. + hertz / 700.)

let of_hertz hertz =
  let mel = to_mel ~hertz in
  { hertz; mel }
