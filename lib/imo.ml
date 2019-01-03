open! Core
open Async
open Std_internal

let target_lines : [ `both | `vertical ] = `both

  (*
    let n_x = 3
    let n_y = 2
    let p =
    let l = P.zero_line_between_two_points in
    P.product
    [ l (0., 0.) (1., 1.)
    ; l (1., 0.) (2., 1.)
    ; l (2., 0.) (0., 1.)
    ]
  *)

let config =
  { Animation.Config.
    n_x = 5
  ; n_y = 4
  ; left_margin = 2
  ; top_margin = 2
  ; right_margin = 5
  ; bottom_margin = 2
  ; style = `heat
  ; cbrange = (-10., 12.)
  ; show_dots = []
  }

let p =
  let l = P.zero_line_between_two_points in
  P.product
    [ l (0., 0.) (4., 2.)
    ; l (0., 1.) (2., 3.)
    ; l (0., 2.) (1., 1.)
    ; l (0., 3.) (4., 1.)
    ; l (1., 3.) (4., 0.)
    ; l (3., 3.) (2., 0.)
    ; l (4., 3.) (1., 0.)
    ; l (3., 0.) (4., 1.)
    ]

  (*&
    let x_margin = 5
    let y_margin = 5
    let n_x = 3
    let n_y = 3
    let p =
    let l = P.zero_line_between_two_points in
    List.init (2 * n_x - 2) ~f:(fun i ->
    let i = float (i + 1) in
    l (0., i) (i, 0.))
    |> P.product
  *)

let lines_to_emerge =
  match target_lines with
  | `both -> Lines.imo_vh config
  | `vertical -> Lines.vertical_lines config

let lines_to_emerge =
  List.map lines_to_emerge ~f:Line.poly

let animate ~dir =
    (* CR-someday: you can skip the last iteration since last two lines fall in
       place together. *)
  let%bind states =
    fold_map_deferred lines_to_emerge
      ~init:(A.State.of_poly p)
      ~f:A.State.emerge
  in
  A.State.interpolate states
  |> A.create ~config
  |> Render_gnuplot.write ~dir
