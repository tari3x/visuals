open Std_internal

type t

module Datum : sig
  type t = V.t * float [@@deriving sexp]

  val weighted_average : t -> t -> w:float -> t
end

module Data : sig
  type t = Datum.t list [@@deriving sexp]
end

val error : P.t -> Data.t -> float

val create
  :  basis:P.t list
  -> Data.t
  -> t

val add : t -> data:Data.t -> t

val result : t -> P.t

val simple : basis:P.t list -> Data.t -> P.t
