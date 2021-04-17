type t =
  { r : float
  ; g : float
  ; b : float
  }
[@@deriving sexp]

val random : unit -> t
val rgb : float -> float -> float -> t
val ( + ) : t -> t -> t
val scale : t -> by:float -> t
val weighted_average : t -> t -> w:float -> t
val black : t
val red : t
val green : t
val blue : t
val graphics : t -> Graphics.color

module Camlimages : sig
  type t = Camlimages_core.Color.rgb =
    { mutable r : int
    ; mutable g : int
    ; mutable b : int
    }
  [@@deriving sexp]

  val black : t
  val red : t
end

val camlimages : t -> Camlimages.t
