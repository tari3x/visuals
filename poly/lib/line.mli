open Core

module V = Vector.Float
module P = V
module Poly = Polynomial

type t = P.t * V.t [@@deriving sexp, compare]

include Comparable with type t := t

val mem : t -> P.t -> bool

val poly : t -> Poly.t

val deform_to_zero : t -> Poly.t -> Poly.t

val deform_to_zero_preserving_boundary : t -> Config.t -> Poly.t -> Poly.t
