open Geometry

(* CR-someday avatar: use non-poly equality  *)

type instr =
  | Not_deeper of float
  | Stop
  | Continue

type 'a t

val create : ?max_depth:int -> ?slice_size:int -> Rectangle.t -> 'a t

(* CR-someday avatar: why would I use the non-tail-recursive version?  *)
val insert : 'a t -> Vector.t -> 'a -> unit
val remove : 'a t -> Vector.t -> 'a -> unit
val clear : 'a t -> unit

val of_list
  :  ?max_depth:int
  -> ?slice_size:int
  -> (Vector.t * 'a) list
  -> 'a t

val fold_rect
  :  'a t
  -> Rectangle.t
  -> init:'b
  -> f:('b -> 'a -> 'b * instr)
  -> 'b
