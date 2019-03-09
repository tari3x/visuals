open Core
open Std_internal

let color =
  Colors.gnuplot_color

let value_color ~(config : Config.t) p_value =
  let open Float in
  let (min_value, max_value) = Config.cbrange config in
  let value_range = max_value - min_value in
  (* Offset it a bit so we are less likely to hit an exact zero in log *)
  let p_value = p_value + 1e-7 in
  let sgn = robust_sign p_value |> Sign.to_float in
  if Float.(sgn = 0.) then color 0.
  else begin
    let value = sgn * log (abs p_value) in
    let value = max value min_value in
    let value = min value max_value in
    let w = (value - min_value) / value_range in
    color w
  end
