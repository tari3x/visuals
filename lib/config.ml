open Core

module V = Vector.Float

(* Point range is [0; n - 1] *)

type t =
  { n_x : int
  ; n_y : int
  ; grid_size : int * int
  ; left_margin : int
  ; top_margin : int
  ; right_margin : int
  ; bottom_margin : int
  ; style : [ `zeroes | `heat ]
  ; cbrange : float * float
  ; show_dots : V.t list
  ; degree : int
  }

let value_steps t =
  let open Float in
  let grid_x, grid_y = t.grid_size in
  let x_step = float t.n_x / float grid_x in
  let y_step = float t.n_y / float grid_y in
  x_step, y_step

