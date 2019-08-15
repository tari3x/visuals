open Base
open Std_internal

val fade_to_base_interpolation_arg : float

module type Elt = sig
  module Id : Id

  type t

  val id : t -> Id.t
  val offset : t -> t -> Vector.t
  val touch : t -> color:Color.t -> flash:bool -> unit
end

module Make(E : Elt) : sig
  module Id : Id

  type t [@@deriving sexp_of]

  val id : t -> Id.t

  val create_exn
    :  other_rains:t list
    -> min_distance:float
    -> elts:E.t list
    -> id:Id.t
    -> config:Config.t
    -> t

  val drop : t -> flash:bool -> unit

  val saturation : t -> float

  val burst : t -> unit Lwt.t
end
