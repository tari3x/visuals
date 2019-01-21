open! Core
open Async
open Std_internal

let config =
  Config.create
    ~grid_size:(7, 5)
    ~cbrange:(-20., 25.) (* (-10, 12) *)
    ()

let imo_lines = Lines.imo_vh config |> List.map ~f:Line.poly

let initial_state () =
  Lines.random_diagonal_cover config
  |> Lines.poly
  |> A.State.of_poly

let animate ~dir =
  let diagonals = initial_state () in
  let%bind states =
    Deferred.List.map imo_lines ~f:(fun line ->
      let%bind state = A.State.emerge diagonals line in
      return (A.State.collapse state))
  in
  A.State.interpolate states
  |> A.create ~config
  |> Render_gnuplot.write ~dir
