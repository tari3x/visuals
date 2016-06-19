open Common

module Kind : sig
  type t =
  | Rect of (float * float)
end

type t

val to_string : t -> string

val create : Point.t -> t

val render : t -> Ctx.t -> unit

val move_by : t -> Point.t -> t

val touched_by : t -> Point.t -> bool
