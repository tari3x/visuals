module BS = Binary_search
open Core
open Std_internal

let color_index ~(config : Config.t) ~num_colors value =
  let open Float in
  let (min_value, max_value) = Config.cbrange config in
  let value_range = max_value - min_value in
  (* Offset it a bit so we are less likely to hit an exact zero in log *)
  let value = value + 1e-7 in
  let sgn = robust_sign value |> Sign.to_float in
  if Float.(sgn = 0.) then Int.(num_colors / 2)
  else begin
    let value = sgn * log (abs value) in
    let value = max value min_value in
    let value = min value max_value in
    let w = (value - min_value) / value_range in
    Int.of_float (w * float num_colors)
  end

module Ctx = struct
  type t = float array

  let create ~config =
    let c = color_index ~config ~num_colors:Colors.num_colors in
    Array.init (Array.length Colors.gnuplot_colors) ~f:(fun i ->
      BS.Float.search
        ~start:Core.Float.(- max_finite_value, max_finite_value)
        ~test:(fun value -> Int.compare i (c value)))

  let get_index t value =
    BS.Int.search
      ~start:(0, Array.length t - 1)
      ~test:(fun i -> Float.compare value t.(i))
end

let value_color (ctx : Ctx.t) value =
  Colors.gnuplot_colors.(Ctx.get_index ctx value)

let value_graphics_color (ctx : Ctx.t) value =
  Colors.graphics_colors.(Ctx.get_index ctx value)
