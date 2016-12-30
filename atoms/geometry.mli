
module Angle : sig
  type t

  val to_string : t -> string

  val zero : t

  val of_degrees : float -> t
  val of_radians : float -> t
  val to_radians : t -> float

  val neg : t -> t

  val (-) : t -> t -> t
  val (+) : t -> t -> t
end

module Vector : sig
  type t

  val create : int -> int -> t
  val coords : t -> (float * float)
  val x : t -> float
  val y : t -> float

  val zero : t

  val length : t -> float
  val angle : t -> Angle.t

  val neg : t -> t
  val (-) : t -> t -> t
  val (+) : t -> t -> t

  val scale : t -> by:float -> t
  val ( / ) : t -> float -> t
  val ( * ) : t -> float -> t

  val to_string : t -> string
end

module Matrix : sig
  type t

  val to_string : t -> string

  val ident : t

  val ( * )   : t -> t -> t
  val ( *> )  : t -> t -> t

  val inv  : t -> t

  val translate : Vector.t -> t
  val rotate : Angle.t -> t
  val scale : scale_x:float -> scale_y:float -> t

  val apply : t -> Vector.t -> Vector.t

  val coeffs : t -> float array array

(* val flatten : t -> float array *)
end

module Frame : sig
  type t

  val to_string : t -> string

  val ( *> )  : t -> t -> t

  val ident : t
  val translate : Vector.t -> t
  val rotate : Angle.t -> t
  val scale : scale_x:float -> scale_y:float -> t

  val scale_viewport : t -> float -> t

  val remove_scale : t -> t

  val equal_scale : t -> t

  val translation : t -> Vector.t
  val set_translation : t -> Vector.t -> t

  val scale_x : t -> float

  val matrix : t -> Matrix.t
end

