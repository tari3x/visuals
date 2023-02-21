(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Core

(** hertz *)
type t = float

let hertz t = t

let mel =
  Memo.general (fun hertz ->
    let open Float in
    1127. * log (1. + (hertz / 700.)))
;;

let greenwood =
  Memo.general (fun hertz ->
    let open Float in
    511. * log (1. + (hertz / 165.4)))
;;

let of_hertz hertz = hertz
