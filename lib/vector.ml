open Core

open Common

module type Ring = sig
  type t [@@deriving sexp, compare]

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
end

module Make (R : Ring) = struct
  module T = struct
    type t = R.t * R.t [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make(T)

  let ( + ) (x1, y1) (x2, y2) : t =
    R.(x1 + x2, y1 + y2)

  let ( - ) (x1, y1) (x2, y2) : t =
    R.(x1 - x2, y1 - y2)

  let cross (x1, y1) (x2, y2) : R.t =
    R.(x1 * y2 - x2 * y1)

  let scale (x, y) scale : t =
    R.(scale * x, scale * y)

  let weighted_average (x1, y1) (x2, y2) ~w =
    let x = weighted_average x1 x2 ~w in
    let y = weighted_average y1 y2 ~w in
    (x, y)
end

module Float = Make(Float)

module Int = struct
  include Make(Int)

  let to_float (x, y) =
    (float x, float y)

  let of_float (x, y) =
    (Int.of_float x, Int.of_float y)
end
