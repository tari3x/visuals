open Core
open Std_internal

let color_index value =
  Int.of_float value

let value_color value =
  Colors.gnuplot_colors.(color_index value)

let value_graphics_color value =
  Colors.graphics_colors.(color_index value)
