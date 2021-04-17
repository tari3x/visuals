open Core
open Common

type t =
  { r : float
  ; g : float
  ; b : float
  }
[@@deriving sexp]

let random () =
  let r = Random.float 1. in
  let g = Random.float 1. in
  let b = Random.float 1. in
  { r; g; b }
;;

let rgb r g b = { r; g; b }
let black = rgb 0. 0. 0.
let red = rgb 1. 0. 0.
let green = rgb 0. 1. 0.
let blue = rgb 0. 0. 1.

let weighted_average t1 t2 ~w =
  let r = weighted_average t1.r t2.r ~w in
  let g = weighted_average t1.g t2.g ~w in
  let b = weighted_average t1.b t2.b ~w in
  { r; g; b }
;;

let ( + ) t1 t2 =
  let open Float in
  let add x1 x2 =
    let x = x1 + x2 in
    min x 1.
  in
  { r = add t1.r t2.r; g = add t1.g t2.g; b = add t1.b t2.b }
;;

let scale t ~by =
  if Float.( <= ) by 0.
  then black
  else if Float.( >= ) by 1.
  then t
  else (
    let { r; g; b } = t in
    let r = r *. by in
    let g = g *. by in
    let b = b *. by in
    { r; g; b })
;;

module Camlimages = struct
  type t = Camlimages_core.Color.rgb =
    { mutable r : int
    ; mutable g : int
    ; mutable b : int
    }
  [@@deriving sexp]

  let black = { r = 0; g = 0; b = 0 }
  let red = { r = 255; g = 0; b = 0 }
end

let c x =
  let open Float in
  Int.of_float (255. * x)
;;

let camlimages { r; g; b } =
  let r = c r in
  let g = c g in
  let b = c b in
  { Camlimages.r; g; b }
;;

let graphics { r; g; b } =
  let r = c r in
  let g = c g in
  let b = c b in
  Graphics.rgb r g b
;;
