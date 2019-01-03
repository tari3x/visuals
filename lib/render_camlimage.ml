open Core
open Std_internal

module A = Animation
module P = Polynomial

module Config = A.Config

let file_id = ref 0

let color =
  Colors.gnuplot_color

(* CR: zeroes still happen. *)
let make_frame ~dir ~config p =
  let open Float in
  let
      { Config.
        n_x
      ; n_y
      ; left_margin = _
      ; top_margin= _
      ; right_margin = _
      ; bottom_margin = _
      ; style = _
      ; cbrange = (min_value, max_value)
      ; show_dots = _
      } = config
  in
  incr file_id;
  let filename = dir ^/ sprintf "frame%06d.png" !file_id in
  let width  = 500 in
  let height = 500 in
  let black = {Color.r = 0; g = 0; b = 0; } in
  let value_range = max_value - min_value in
  let step_x = float n_x / float width in
  let step_y = float n_y / float height in
  let image = Rgb24.make width height black in
  for i = 0 to Int.(width - 1) do
    for j = 0 to Int.(height - 1) do
      let x = float i * step_x in
      let y = float Int.(height - j - 1) * step_y in
      let p_value = P.eval p [x; y] in
      let sign = robust_sign p_value |> Sign.to_float in
      let color =
        if p_value = 0. then color 0.
        else begin
          let value = sign * log (abs p_value) in
          let value = max value min_value in
          let value = min value max_value in
          let w = (value - min_value) / value_range in
          color w
        end
      in
      Rgb24.set image i j color;
    done;
  done;
  Png.save filename [] (Images.Rgb24 image)

let write ~dir { A. config; states } =
  List.iter states ~f:(fun { A.State. p; ps; defs = _; dots = _ } ->
    let p = P.product (p :: ps) in
    make_frame ~dir ~config p);
  Async.return ()
