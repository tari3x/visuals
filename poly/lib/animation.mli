open Async

module P = Polynomial
module E = Maxima.Expr
module V = Vector.Float

module State : sig
 type t =
   { p : P.t
   ; ps : P.t list
   ; defs : (string * P.t) list
   ; dots : V.t list
   ; palette : Palette.Basis.t
   }

  val of_poly
    :  ?show_dots:V.t list
    -> P.t
    -> t

  val with_palette : t -> palette:Palette.Basis.t -> t

  val collapse : t -> t

  (** Only works on horizontal and vertical lines. *)
  val emerge : t -> Line.t -> t Deferred.t

  val interpolate : t list -> t list

  val suspend_end : ?num_frames:int -> t list -> t list
end

type t =
  { config : Config.t
  ; states : State.t list
  } [@@deriving sexp]

val create : config:Config.t -> State.t list -> t
