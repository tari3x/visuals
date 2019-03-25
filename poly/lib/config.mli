module Style : sig
  type t = [ `zeroes | `heat ]
end

type t

val create
  :  grid_size:int * int
  -> ?image_width : int
  -> cbrange:float * float
  -> ?style:Style.t
  -> unit
  -> t

val grid_size : t -> int * int
val image_size : t -> int * int
val domain_size : t -> float * float

val domain_centre : t -> Vector.Float.t

val domain : t -> Vector.Float.t * Vector.Float.t

val domain_to_image
  :  t
  -> Vector.Float.t
  -> Vector.Int.t

val image_to_domain
  :  t
  -> Vector.Int.t
  -> Vector.Float.t

val style : t -> Style.t

val cbrange : t -> float * float
