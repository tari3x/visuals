open Core
open Std_internal

let degree = 15

let config =
  Config.create
    ~grid_size:(10, 10)
    ~cbrange:(-15., 15.)
    ~image_width:1500
    ()

let set_value x =
  List.map ~f:(fun p -> (p, x))

let grid_points =
  Lines.all_points config
  |> List.map ~f:Vector.Int.to_float

let grid =
  set_value 0. grid_points

let random n =
  let range_mult = 1. in
  let n_x, n_y = Config.grid_size config in
  let n_x = float n_x in
  let n_y = float n_y in
  List.init n ~f:(fun _ ->
    let random range =
      Float.(-range * (range_mult - 1.) / 2.
             + Random.float (range_mult * range))
    in
    let x = random n_x in
    let y = random n_y in
     (* let value = Float.(-200. + Random.float 400.) in *)
    let value = 100. in
    ((x, y), value))

let animate ~dir =
  let open P in
  let data = grid @ random 10 in
  let p = P.lagrange data ~degree in
  let ps =
    p :: List.concat_map (P.monomials p) ~f:(fun p_m ->
      [p - p_m; p])
  in
  let states = List.map ps ~f:A.State.of_poly in
  A.State.interpolate states
  |> A.create ~config
  |> Render_camlimage.write ~dir
