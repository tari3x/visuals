open Core
open Std_internal

let num_colors = Colors.num_colors

let color_index value =
  Int.of_float (value *. float (num_colors - 1))

let value_color value =
  Colors.gnuplot_colors.(color_index value)

let value_graphics_color value =
  Colors.graphics_colors.(color_index value)
