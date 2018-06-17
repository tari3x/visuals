(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common

module Node = struct
  type t = Time.t * Color.t [@@deriving sexp]
end

(* Invariants:
   1. Not empty.
   2. First time is in the past.
*)
type t = Node.t list [@@deriving sexp]

let start_now color : t =
  [ Time.now (), color ]

let add (t : t) ~after ~color : t =
  let (last, _) = List.last_exn t in
  t @ [ Time.(last + after), color ]

let eval t =
  let now_ = Time.now () in
  let rec eval = function
    | [] -> failwith "BUG: Color_flow empty"
    | [_, color] -> color
    | (time1, color1) :: (time2, color2) :: t ->
      let open Float in
      let time1 = Time.to_sec time1 in
      let time2 = Time.to_sec time2 in
      let now_ = Time.to_sec now_ in
      if now_ > time2 then eval (List.tl_exn t)
      else if time1 = time2 then color1
      else Color.interpolate_two
        color1 color2
        ((now_ - time1) / (time2 - time1))
  in
  eval t

