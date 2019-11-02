open Core

module V = Vector.Float
module P = Polynomial

type t =
  { center : V.t
  ; radius : float
  ; angles : float array
  ; speeds : float array
  ; values : float array
  }

let create ~center ~radius =
  let open Float in
  let angles = Array.init 1000 ~f:(fun _ -> Random.float (2. * Float.pi)) in
  let speeds = Array.init 1000 ~f:(fun _ -> 1. + Random.float 0.1) in
  let values = Array.init 1000 ~f:(fun i ->
    if Int.(i % 2 = 0) then -100. else 100.)
  in
  { center
  ; radius
  ; angles
  ; speeds
  ; values
  }

let frame t ~num_segments ~num_points_per_segment ~phase =
  let open Float in
  List.init num_points_per_segment ~f:(fun i ->
    (* let angle = 2. * pi / 10. * float i in *)
    let angle = t.angles.(i) / float num_segments in
    let speed = t.speeds.(i) in
    let value = t.values.(i) in
    let point j =
      let angle = angle * float Int.(j + 1) in
      let direction = (cos angle, sin angle) in
      let offset = V.scale direction (t.radius * sin (phase * speed)) in
      V.(t.center + offset), value
    in
    List.init num_segments ~f:point
  )
  |> List.concat
