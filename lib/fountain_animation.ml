open Core
open Std_internal

let n_x = 5
let n_y = 5

let config =
  Config.create
    ~grid_size:(n_x, n_y)
    ~cbrange:(-15., 15.)
    ()

let num_steps = 100

let center =
  (float n_x /. 2., float n_y /. 2.)

let radius =
  Float.max (float n_x) (float n_y) (*  /. 2. *)

let set_value x =
  List.map ~f:(fun p -> (p, x))

let grid =
  Lines.all_points config
  |> List.map ~f:Vector.Int.to_float
  |> set_value 0.

let animate ~dir =
  let fountain = Fountain.create ~center ~radius in
  let states =
    List.init num_steps ~f:(fun step ->
      let data =
        Fountain.frame
          fountain
          ~num_segments:6
          ~num_points_per_segment:10
          ~phase:(float (step + 12000) /. 40.)
      in
      let p = P.lagrange data in
      let _points = List.map data ~f:fst in
      A.State.of_poly p (* ~show_dots:_points *)
    )
  in
  A.State.interpolate states
  |> A.create ~config
  |> Render_gnuplot.write ~dir

