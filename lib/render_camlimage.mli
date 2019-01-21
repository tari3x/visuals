open Async

val value_color : config:Config.t -> float -> Color.Rgb.t

val write
  :  dir:string
  -> Animation.t
  -> unit Deferred.t
