open Async

val write
  :  dir:string
  -> Animation.t
  -> unit Deferred.t

val command : Command.t
