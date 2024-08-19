open Std_internal
module Shape = Shapes.Elt
module Id : Id

type t [@@deriving sexp_of]

val id : t -> Id.t

val create_exn
  :  id:Id.t
  -> config:Config.Rain.t
  -> other_rains:t list
  -> shapes:Shapes.t
  -> touch:(Shape.t -> color:Color.t -> flash:bool -> unit)
  -> t

val drop : t -> flash:bool -> unit
val burst : t -> drops_at_once:int -> unit Lwt.t
val saturation : t -> float
