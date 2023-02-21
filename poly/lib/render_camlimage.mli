open Async
open Std_internal

module Ctx : sig
  type t

  val create : Config.t -> t
end

val write_image
  :  ctx:Ctx.t
  -> dir:string
  -> palette:Palette.t
  -> ?dots:V.t list
  -> P.t
  -> unit

val write : dir:string -> Animation.t -> unit Deferred.t
val command : Command.t
