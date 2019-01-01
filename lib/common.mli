open Core
open Async

val weighted_average : float -> float -> w:float -> float

val interpolate
  :  weighted_average:(w:float -> 'a -> 'a -> 'a)
  -> num_steps:int
  -> 'a list
  -> 'a list

val fold_map_deferred
  :  'a list
  -> init:'b
  -> f:('b -> 'a -> 'b Deferred.t)
  -> 'b list Deferred.t

val intercalate : 'a list -> 'a list -> 'a list

module Float : sig
  include module type of Float

  val sum : t list -> t
  val product : t list -> t
end

module Int : sig
  include module type of Int

  val sum : t list -> t
  val product : t list -> t
end

val debug : enabled:bool -> ('a, unit, string, unit) format4 -> 'a

module List : sig
  include module type of List

  val product : 'a t t -> 'a t t
end
