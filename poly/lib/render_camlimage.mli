open Async
open Std_internal
open Float_array

val write_image
  :  dir:string
  -> config:Config.t
  -> values:A2_int.t
  -> palette:Palette.t
  -> ?dots:V.t list
  -> unit
  -> unit

val write
  :  dir:string
  -> Animation.t
  -> unit Deferred.t

val command : Command.t
