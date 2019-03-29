(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common

type t =
  { r : int
  ; g : int
  ; b : int
  ; a : float
  } [@@deriving sexp]

let create ~r ~g ~b ~a =
  { r; g; b; a }

let r t = t.r
let g t = t.g
let b t = t.b
let a t = t.a

let white = create ~r:255 ~g:255 ~b:255 ~a:1.
let black = create ~r:0 ~g:0 ~b:0 ~a:1.
let red = create ~r:255 ~g:0 ~b:0 ~a:1.
let green = create ~r:0 ~g:255 ~b:0 ~a:1.
let blue = create ~r:0 ~g:0 ~b:255 ~a:1.
let magenta = create ~r:255 ~g:0 ~b:255 ~a:1.
let cyan = create ~r:0 ~g:255 ~b:255 ~a:1.
let yellow = create ~r:255 ~g:255 ~b:0 ~a:1.
let dark_orange = create ~r:255 ~g:140 ~b:0 ~a:1.

(*
let wave ~cycle time =
  ((sin (time *. cycle) +. 1.) /. 2.) *. 255. |> int

let of_time time =
  let r = wave ~cycle:1. time in
  let g = wave ~cycle:2. time in
  let b = wave ~cycle:3. time in
  let a = 0.7 in
  { r; g; b; a }
*)

let to_string { r; g; b; a } =
  Printf.sprintf "rgba(%d, %d, %d, %f)" r g b a

let of_hex8_string s =
  let get n =
    Printf.sprintf "0X%s" (String.sub s ~pos:n ~len:2) |> Int.of_string
  in
  { r = get 3
  ; g = get 5
  ; b = get 7
  ; a = (Float.of_int (get 1)) /. 255.
  }

let random () =
  let r = Random.int 255 in
  let g = Random.int 255 in
  let b = Random.int 255 in
  let a = 1. in
  { r; g; b; a }

let interpolate_two t1 t2 fraction =
  let i n1 n2 =
    (1. -. fraction) *. (Float.of_int n1) +. fraction *. (Float.of_int n2)
    |> Int.of_float
  in
  { r = i t1.r t2.r
  ; g = i t1.g t2.g
  ; b = i t1.b t2.b
  ; a = (1. -. fraction) *. t1.a +. fraction *. t2.a
  }

(* CR-someday: rename fraction to arg everywhere. *)
let interpolate ts ~arg:fraction =
  match ts with
  | [] -> white
  | [ t ] -> t
  | ts ->
    if Float.(fraction <= 0.) then List.hd_exn ts
    else if Float.(fraction >= 1.) then List.last_exn ts
    else begin
      let segment_length = 1. /. (Float.of_int (List.length ts - 1)) in
      let fraction_of_segment =
        (Float.mod_float fraction segment_length) /. segment_length
      in
      let segment_number =
        Int.of_float (Float.round_down (fraction /. segment_length))
      in
      let c1 = List.nth_exn ts segment_number in
      let c2 = List.nth_exn ts (segment_number + 1) in
      interpolate_two c1 c2 fraction_of_segment
    end

let set_alpha t ~alpha =
  { t with a = alpha }

let scale t scale =
  if Float.(<=) scale 0. then black
  else if Float.(>=) scale 1. then t
  else begin
    let { r; g; b; a } = t in
    let r = int ((float r) *. scale) in
    let g = int ((float g) *. scale) in
    let b = int ((float b) *. scale) in
    { r; g; b; a }
  end

let maximize t =
  let max_component =
    List.max_elt ~compare:Int.compare [ t.r; t.g; t.b ]
    |> Option.value_exn
    |> float
  in
  let s = Float.(255. / max_component) in
  scale t s
