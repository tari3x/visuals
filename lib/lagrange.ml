open Core
open Async
open Std_internal

let config =
  { Animation.Config.
    n_x = 10
  ; n_y = 10
  ; left_margin = 0
  ; top_margin = 0
  ; right_margin = 0
  ; bottom_margin = 0
  ; style = `heat
  ; cbrange = (-15, 15)
  ; show_dots = []
  }

  (*
    let num_random = 1
    let change_per_step = 1
  *)
  (* A single step is >3seconds *)
let num_steps = 20
let segment_length = 10
(* let num_steps = 2000 *)
  (* spreading them out more makes it more washed out. *)
let range_mult = 1.
let degree = 15

let set_value x =
  List.map ~f:(fun p -> (p, x))

let grid =
  Lines.all_points config
  |> List.map ~f:Vector.Int.to_float
  |> set_value 0.

let random n =
  let { Animation.Config. n_x; n_y; _ } = config in
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

let weighted_average ~w points1 points2 =
  List.map2 points1 points2 ~f:(P.Datum.weighted_average ~w)
  |> function
  | List.Or_unequal_lengths.Ok points -> points
  (* CR: *)
  | Unequal_lengths -> points1

let perturbations =
  let open Float in
  let ps = grid @ random 10 in
  let p1 = (2.7, 2.3) in
  let p2 = (3.5, 3.3) in
  let frame value ~w =
    let p = Vector.Float.weighted_average p1 p2 ~w in
    (p, value) :: ps
  in
  (*
  List.init 100 ~f:(fun i ->
    frame (float i) ~w:0.)
  @
  *)
  let make step =
    List.init 10 ~f:(fun i ->
      frame 100. ~w:(step * float i))
  in
  make 1e-14
  @ make 1e-13
  @ make 1e-12
  @ make 1e-11
  @ make 1e-10
  @ make 1e-9
  @ make 1e-8
  @ make 1e-7
  @ make 1e-6
  @ make 1e-5

let animate ~dir =
  (*
  let segment () =
    let num_random = 1 + Random.int 36 in
    (* let num_random = 3 in *)
    let change_per_step = 2 in
    (*
    let change_per_step =
      1 +
        if Random.int 2 = 0
        then Random.int (min 4 num_random)
        else Random.int num_random
    in
    *)
    let rec gen_points i ps =
      if i = 0 then []
      else begin
        let ps = List.drop ps change_per_step @ random change_per_step in
        (grid @ ps) :: gen_points (i - 1) ps
      end
    in
    gen_points segment_length (random num_random)
  in
  let points =
    List.init (num_steps / segment_length) ~f:(fun _ -> segment ())
    |> List.concat
    |> interpolate ~weighted_average ~num_steps:100
  in
  *)
  let points = perturbations in
  let%bind states =
    Deferred.List.map points ~f:(fun data ->
      let%bind p = P.lagrange ~degree data in
      A.State.create p (*  ~show_dots:(List.map data ~f:fst) *)
      |> return)
  in
  A.write ~dir ~config ~interpolate:false states
