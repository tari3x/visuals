open Core
open Async

include module type of Int.Replace_polymorphic_compare

module type Ring = sig
  type t [@@deriving sexp, compare, equal]

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
end

val weighted_average : float -> float -> w:float -> float

(** Does not include the last element. *)
val interpolate
  :  weighted_average:(w:float -> 'a -> 'a -> 'a)
  -> num_steps:int
  -> 'a list
  -> 'a list

(** Does not include the last element. *)
val interpolate_pipe
  :  weighted_average:(w:float -> 'a -> 'a -> 'a)
  -> num_steps:int
  -> 'a Pipe.Reader.t
  -> 'a Pipe.Reader.t

module Float : sig
  include module type of Float

  val sum : t list -> t
  val product : t list -> t
  val average_exn : t list -> t
  val sign_val : t -> t
end

module Int : sig
  include module type of Int

  val sum : t list -> t
  val product : t list -> t
  val factorial : t -> t
end

val debug : enabled:bool -> ('a, unit, string, unit) format4 -> 'a

val debug_s : enabled:bool -> Sexp.t -> unit

module List : sig
  include module type of List

  val product : 'a t t -> 'a t t

  val take : 'a t -> n:int -> 'a t

  val scan
    :  'a t
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> 'b t

  val scan_deferred
    :  'a t
    -> init:'b
    -> f:('b -> 'a -> 'b Deferred.t)
    -> 'b t Deferred.t

  val intercalate : 'a t -> 'a t -> 'a t
end
