open Core
open Async
open Std_internal

(* CR: make sure lines don't repeat. Keep some straight lines to keep it
   interesting. *)

let config =
  { Animation.Config.
    n_x = 7
  ; n_y = 5
  ; left_margin = 0
  ; top_margin = 0
  ; right_margin = 0
  ; bottom_margin = 0
  ; style = `heat
  ; cbrange = (-20., 25.)
  ; show_dots = []
  }

let n_lines = 15
let n_steps = 50

let rec random_line () =
  let point () =
    let x = Random.int config.n_x in
    let y = Random.int config.n_y in
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
