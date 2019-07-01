open Core
open Common

module Make (R : Ring) = struct
  type t = R.t * R.t [@@deriving sexp, compare, equal]

  (* include Comparable.Make(T) *)

  let (=) = equal

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

module Float = struct
  include Make(Float)

  let length (x, y) =
    Float.(sqrt (x**2. + y**2.))
end

module Int = struct
  include Make(Int)

  let to_float (x, y) =
    (float x, float y)

  let of_float (x, y) =
    (Int.of_float x, Int.of_float y)
end
