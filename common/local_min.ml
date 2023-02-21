(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base

module Dir = struct
  type t =
    | Up
    | Down
end

type t =
  { mutable last_dir : Dir.t
  ; mutable last : float
  ; mutable min : float
  }

let create () = { last_dir = Up; last = 0.; min = 0. }

let add t value =
  let open Float in
  let dir : Dir.t =
    if value > t.last
    then Up
    else if value < t.last
    then Down
    else t.last_dir
  in
  let min =
    match t.last_dir, dir with
    | Down, Up -> value
    | _ -> t.min
  in
  t.last_dir <- dir;
  t.last <- value;
  t.min <- min
;;

let get t = t.min
