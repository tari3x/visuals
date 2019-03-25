
module Color : sig
  type t = Color.rgb [@@deriving sexp]

  val graphics_color : t -> Graphics.color
end

val num_colors : int
val gnuplot_colors : Color.t array
val graphics_colors : Graphics.color array

val black : Color.t

val red : Color.t
