include Bigarray

module A1 = struct
  include (Array1 : module type of Array1
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) Array1.t)

  type t = (float, float64_elt, c_layout) Array1.t

  let create = create float64 C_layout
end

module A2 = struct
  include (Array2 : module type of Array2
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) Array2.t)

  type t = (float, float64_elt, c_layout) Array2.t

  let create = create float64 C_layout

  let get_flipped t i j =
    let j = dim2 t - j - 1 in
    t.{i, j}
end

module A3 = struct
  include (Array3 : module type of Array3
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) Array3.t)

  type t = (float, float64_elt, c_layout) Array3.t

  let create = create float64 C_layout
end
