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
