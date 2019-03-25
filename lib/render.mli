
module Ctx : sig
  type t

  val create : config:Config.t -> t
end

val value_color          : Ctx.t -> float -> Color.Rgb.t
val value_graphics_color : Ctx.t -> float -> Graphics.color
