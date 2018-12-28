open Async

module Expr : sig
  type t [@@deriving sexp]

  val to_gnuplot : t -> string
  val to_string : t -> string

  val var : int -> t

  val func : string -> t list -> t
  val const : float -> t

  val ( * ) : t -> t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t

  val pow : t -> int -> t

  val product : t list -> t
  val sum : t list -> t

  val scale : t -> float -> t

  val ev : t -> t list -> t

  val expand : t -> t

  val eval : t -> t Deferred.t

  (* quotient and remainder *)
  module Division_result : sig
    type nonrec t = { q : t; r : t }
  end

  val divide : t -> t -> Division_result.t Deferred.t
end

module Matrix : sig
  type t [@@deriving sexp]

  val create : Expr.t list list -> t

  val det : t -> Expr.t

  val eval : t -> t Deferred.t
end
