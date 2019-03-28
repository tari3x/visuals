open Core
open Std_internal

let _debug a = debug_s ~enabled:true a

let value_color value =
  Colors.gnuplot_colors.(value)

let value_graphics_color value =
  Colors.graphics_colors.(value)
