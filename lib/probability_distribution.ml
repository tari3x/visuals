(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common

module Elt = struct
  type 'a t = 'a * float [@@deriving sexp]

  let create_exn ~weight x =
    let open Float in
    if weight < 0.
    then failwithf "PD.create: negative weight %f" weight ()
    else if weight = 0.
    then None
    else Some (x, weight)
  ;;

  let weight = snd
end

module Elts = struct
  type 'a t =
    { elts : 'a Elt.t list
    ; total : float
    }
  [@@deriving sexp]

  let create_exn (elts : 'a Elt.t list) : 'a t =
    let open Float in
    let total = List.sum (module Float) elts ~f:snd in
    if total = 0. then failwith "PD.create: all weights are zero";
    { elts; total }
  ;;

  let draw (t : _ t) =
    let open Float in
    let r = Random.float t.total in
    let rec loop acc = function
      | [] -> assert false
      | (value, weight) :: elts ->
        let acc = acc + weight in
        if acc >= r then value else loop acc elts
    in
    loop 0. t.elts
  ;;
end

(* CR-someday: optimize *)
type 'a t =
  | List of 'a Elts.t
  | Tree of 'a t Elts.t
[@@deriving sexp]

let create_exn t = List (Elts.create_exn t)

let rec draw = function
  | List xs -> Elts.draw xs
  | Tree ts -> Elts.draw ts |> draw
;;

(* CR-someday: figure out how to create unit tests. *)
let () =
  let t = create_exn [ 1, 0.; 2, 10.; 3, 0. ] in
  assert (draw t = 2)
;;

let () =
  let t = create_exn [ 1, 10.; 2, 0.; 3, 0. ] in
  assert (draw t = 1)
;;

let () =
  let t = create_exn [ 1, 0.; 2, 0.; 3, 10. ] in
  assert (draw t = 3)
;;
