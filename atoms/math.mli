
open Common

module Matrix : sig
  type t

  val create : float array array -> t

  val apply : t -> Point.t -> Point.t

  val flatten : t -> float array
end

val lusolve : Matrix.t -> float array -> float array
