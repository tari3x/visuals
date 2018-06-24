(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common

(* CR: optimize *)
type 'a t = ('a * float) list [@@deriving sexp]

let () =
  Random.self_init ()

let create_exn t =
  let open Float in
  let t = List.filter t ~f:(fun (_, weight) ->
    if weight < 0. then failwithf "PD.create: negative weight %f" weight ();
    weight <> 0.)
  in
  let total, t =
    List.fold_map t ~init:0. ~f:(fun total (value, weight) ->
      let total = total + weight in
      total, (value, total))
  in
  if total = 0. then failwith "PD.create: all weights are zero";
  List.map t ~f:(fun (value, boundary) ->
      (value, boundary / total))

let draw t =
  let open Float in
  let r = Random.float 1. in
  List.find_map_exn t ~f:(fun (value, boundary) ->
    if boundary >= r then Some value else None)

(* CR-someday: figure out how to create unit tests. *)
let () =
  let t = create_exn
    [ 1, 0.
    ; 2, 10.
    ; 3, 0. ]
  in
  assert (draw t = 2)

let () =
  let t = create_exn
    [ 1, 10.
    ; 2, 0.
    ; 3, 0. ]
  in
  assert (draw t = 1)

let () =
  let t = create_exn
    [ 1, 0.
    ; 2, 0.
    ; 3, 10. ]
  in
  assert (draw t = 3)
