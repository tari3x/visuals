open Core

module P = Polynomial
module Point = Vector

module T = struct
  type t = Point.Int.t * Vector.Int.t [@@deriving sexp, compare]
end

include T

let mem (p0, v) p =
  Vector.Int.(cross (p - p0) v) = 0

let poly (p0, v) =
  let open Point.Int in
  P.zero_line_between_two_points
    (to_float p0)
    (to_float (p0 + v))

include Comparable.Make(T)
