open Core
open Async
open Std_internal

(* CR: make sure lines don't repeat. Keep some straight lines to keep it
   interesting. *)

let n_x = 7
let n_y = 5

let config =
  Config.create
    ~grid_size:(n_x, n_y)
    ~cbrange:(-20., 25.)
    ()

let n_lines = 15
let n_steps = 50

let rec random_line () =
  let point () =
    let x = Random.int n_x in
    let y = Random.int n_y in
    (float x, float y)
  in
  let p1 = point () in
  let p2 = point () in
  if Point.Float.(p1 = p2)
  then random_line ()
  else P.zero_line_between_two_points p1 p2

  (*
    let initial_state () =
    List.init n_lines ~f:(fun _ -> random_line ())
    |> P.product
    |> A.State.of_poly
  *)
let initial_state () =
  Lines.random_diagonal_cover config
  |> Lines.poly
  |> A.State.of_poly

let animate ~dir =
  let lines = List.init n_steps ~f:(fun _ ->
    Lines.random_regular_line config |> Line.poly)
  in
  let%bind states =
    fold_map_deferred lines
      ~init:(initial_state ())
      ~f:(fun state l ->
        let%bind state = A.State.emerge state l in
        let state = A.State.collapse state in
        return state)
  in
  A.State.interpolate states
  |> A.create ~config
  |> Render_gnuplot.write ~dir
