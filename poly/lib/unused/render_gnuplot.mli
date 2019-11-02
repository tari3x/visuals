open Async

val write
  :  dir:string
  -> ?batch_size:int
  -> Animation.t
  -> unit Deferred.t
