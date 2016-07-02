
type t

val create : r:int -> g:int -> b:int -> a:float -> t

val r : t -> int
val g : t -> int
val b : t -> int
val a : t -> float

val to_string : t -> string

val white : t

val of_hex8_string : string -> t

val random : unit -> t

val interpolate : t -> t -> float -> t

val set_alpha : t -> alpha:float -> t

(*
val red : t -> int
val green : t -> int
val blue : t -> int
val alpha : t -> float
*)
