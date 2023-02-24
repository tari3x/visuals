open Base
open Std_internal

module type Elt = sig
  module Id : Id

  type t

  val id : t -> Id.t
  val offset : t -> t -> Vector.t
  val touch : t -> color:Color.t -> flash:bool -> unit
end

module Make (E : Elt) : sig
  module Id : Id

  type t [@@deriving sexp_of]

  val id : t -> Id.t

  val create_exn
    :  other_rains:t list
    -> step:float
    -> elts:E.t list
    -> id:Id.t
    -> config:Config.Rain.t
    -> t

  val drop : t -> flash:bool -> unit
  val burst : t -> drops_at_once:int -> unit Lwt.t
  val saturation : t -> float
end
