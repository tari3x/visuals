module P = Polynomial
module Win = Sdl_window

type t

val create : Config.t -> t
val update_palette : t -> Palette.t -> unit
val draw : t -> P.t -> unit
val poll_event : t -> Win.Event.t option
