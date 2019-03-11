
module V = Vector.Float
module P = Polynomial

type t

val create : center:V.t -> radius:float -> t

val frame
  :  t
  -> num_segments:int
  -> num_points_per_segment:int
  -> phase:float
  -> Lagrange.Datum.t list
