
module Color : sig
  type t = Color.rgb [@@deriving sexp]
end

val gnuplot_color : float -> Color.t

val black : Color.t

val red : Color.t
