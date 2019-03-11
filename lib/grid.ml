open Core
open Async
open Std_internal

module L = Lagrange

let debug a = debug ~enabled:true a

let degree = 15

let config =
  Config.create
    ~grid_size:(10, 10)
    ~cbrange:(-15., 15.)
    ~image_width:896
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

let _weighted_average ~w points1 points2 =
  List.map2 points1 points2 ~f:(Lagrange.Datum.weighted_average ~w)
  |> function
  | List.Or_unequal_lengths.Ok points -> points
  (* CR: *)
  | Unequal_lengths -> points1

let perturbations () =
  let static_points = grid @ random 10 in
  let static = Data.create static_points ~degree in
  let p1 = (-10., 4.5) in
  let p2 = (20., 4.5) in
  debug !"%{Sexp#hum}" [%message (static : Data.t)];
  debug !"%{Sexp#hum}" [%message (p1 : V.t) (p2 : V.t)];
  let data value ~w =
    debug "computing lagrange for w = %f" w;
    let p = Vector.Float.weighted_average p1 p2 ~w in
    Data.add_data static ~data:[p, value]
    |> Data.set_desc ~desc:[%message (value : float) (w : float)]
    (* |> Data.set_dots ~dots:[p] *)
  in
  let open Float in
  (* let num_steps = V.length (p1 - p2) / step in *)
  let num_steps = 3000 in
  let step = 1. / float num_steps in
  List.init num_steps ~f:(fun i ->
    data 0. ~w:(step * float i))

let animate ~dir =
  let data = perturbations () in
  let (states, errors) =
    List.map data ~f:(fun {Data. data; lagrange; desc = _; show_dots } ->
      let p = L.result lagrange in
      let error = L.error p data in
      A.State.of_poly p ~show_dots,
      error)
    |> List.unzip
  in
  debug "total error = %f" (Float.sum errors);
  let%bind () =
    A.create ~config states
    |> Render_camlimage.write ~dir ~degree
  in
  return ()
