open Core
open Async
open Std_internal

let debug a = debug_s ~enabled:true a

module L = Lagrange

let degree = 1

let config =
  Config.create
    ~grid_size:(10, 10)
    ~cbrange:(-15., 15.)
    ~image_width:896
    ~rendering_degree:degree
    ()

let flyby () =
  let open Float in
  let basis = P.[ const 500.; y; x ] in
  let data ~w : Data.t =
    debug [%message (w : float)];
    let p1 = (5., 5.) in
    let p2 = V.weighted_average (6., 7.) (6., 3.) ~w in
    [ (p1, 100.); (p2, 0.) ]
    |> Data.create ~degree ~basis
  in
  let num_steps = 100 in
  let step = 1. / float num_steps in
  List.init num_steps ~f:(fun i ->
    data ~w:(step * float i)
  )

let animate ~dir =
  let data = flyby () in
  let states =
    List.map data ~f:(fun {Data. data = _; lagrange; desc = _; show_dots } ->
      let p = L.result lagrange in
      A.State.of_poly p ~show_dots)
    |> List.take ~n:1
  in
  let%bind () =
    A.create ~config states
    |> Render_camlimage.write ~dir
  in
  return ()
