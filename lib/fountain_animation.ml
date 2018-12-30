open Core
open Async
open Std_internal

let config =
  { Animation.Config.
    n_x = 5
  ; n_y = 5
  ; left_margin = 0
  ; top_margin = 0
  ; right_margin = 0
  ; bottom_margin = 0
  ; style = `heat
  ; cbrange = (-15, 15)
  ; show_dots = []
  }

let num_steps = 100

let center =
  (float config.n_x /. 2., float config.n_y /. 2.)

let radius =
  Float.max (float config.n_x) (float config.n_y) (*  /. 2. *)

let set_value x =
  List.map ~f:(fun p -> (p, x))

let grid =
  Lines.all_points config
  |> List.map ~f:Vector.Int.to_float
  |> set_value 0.

let animate ~dir =
  let fountain = Fountain.create ~center ~radius in
  let%bind states =
    Deferred.List.init num_steps ~f:(fun step ->
      let data =
        Fountain.frame
          fountain
          ~num_segments:6
          ~num_points_per_segment:10
          ~phase:(float (step + 12000) /. 40.)
      in
      let%bind p = P.lagrange ~degree:10 data in
      let _points = List.map data ~f:fst in
      A.State.of_poly p (* ~show_dots:_points *)
      |> return)
  in
  A.write ~dir ~config ~interpolate:true states

