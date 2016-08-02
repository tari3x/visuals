
type t =
  { colors : Color.t list
  (* in seconds *)
  ; length : float
  (* 0 <= offset <= 1 is the fraction of the cycle. *)
  ; offset : float
  }

val const : Color.t -> t

val to_string : t -> string

val default : t

val random : unit -> t

val random_constant : unit -> t

val current_color : t -> time:float -> Color.t

val nth_defaulting_to_last_or_white : t -> int -> Color.t

val set_alpha : t -> alpha:float -> t
