open Async

val write
  :  dir:string
  -> degree:int
  -> Animation.t
  -> unit Deferred.t
