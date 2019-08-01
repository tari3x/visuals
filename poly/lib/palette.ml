open Core
open Async
open Common

module Color = struct
  type t = Color.rgb =
    { mutable r : int
    ; mutable g : int
    ; mutable b : int
    } [@@deriving sexp]

  let weighted_average t1 t2 ~w =
    let r1 = Float.of_int t1.r in
    let g1 = Float.of_int t1.g in
    let b1 = Float.of_int t1.b in
    let r2 = Float.of_int t2.r in
    let g2 = Float.of_int t2.g in
    let b2 = Float.of_int t2.b in
    let r = weighted_average r1 r2 ~w |> Int.of_float in
    let g = weighted_average g1 g2 ~w |> Int.of_float in
    let b = weighted_average b1 b2 ~w |> Int.of_float in
    { r; g; b }

  let black = { r = 0; g = 0; b = 0; }
  let red   = { r = 255; g = 0 ; b = 0 }
  let green = { r = 0; g = 255 ; b = 0 }
  let blue  = { r = 0; g = 0 ; b = 255 }

  let graphics_color { r; g; b } =
    Graphics.rgb r g b

  let c x =
    let open Float in
    Int.of_float (255. * x)

  let of_weights (r, g, b) =
    let r = c r in
    let g = c g in
    let b = c b in
    { r; g; b }

  let (+) t1 t2 =
    let add x1 x2 =
      let x = x1 + x2 in
      min x 255
    in
    { r = add t1.r t2.r
    ; g = add t1.g t2.g
    ; b = add t1.b t2.b
    }

  let scale t ~by =
    if Float.(<=) by 0. then black
    else if Float.(>=) by 1. then t
    else begin
      let { r; g; b } = t in
      let r = Int.of_float ((float r) *. by) in
      let g = Int.of_float ((float g) *. by) in
      let b = Int.of_float ((float b) *. by) in
      { r; g; b }
    end
end

open Color

module Basis = struct
  type t = Color.t * Color.t * Color.t [@@deriving sexp]

  let weighted_average ~w (r1, g1, b1) (r2, g2, b2) =
    let r = Color.weighted_average r1 r2 ~w in
    let g = Color.weighted_average g1 g2 ~w in
    let b = Color.weighted_average b1 b2 ~w in
    (r, g, b)

  let rgb =
    (red, green, blue)

  let split_unit () =
    let open Float in
    let x1 = Random.float 1. in
    let x2 = Random.float 1. in
    let x3 = Random.float 1. in
    let total = x1 + x2 + x3 in
    let x1 = x1 / total in
    let x2 = x2 / total in
    let x3 = x3 / total in
    (x1, x2, x3)

  let random () =
    let (r1, r2, r3) = split_unit () in
    let (g1, g2, g3) = split_unit () in
    let (b1, b2, b3) = split_unit () in
    Color.of_weights (r1, g1, b1),
    Color.of_weights (r2, g2, b2),
    Color.of_weights (r3, g3, b3)

  let pipe config =
    let num_steps = Float.(100. / Config.speed config) |> Int.of_float in
    let ts =
      [| (red, green, blue)
      ; (red, blue, green)
      ; (green, red, blue)
      ; (green, blue, red)
      ; (blue, red, green)
      ; (blue, green, red)
      |]
    in
    let current = ref 0 in
    let static = Pipe.create_reader ~close_on_exception:false (fun writer ->
      Deferred.forever () (fun () ->
        incr current;
        Pipe.write writer ts.(!current % Array.length ts));
      Deferred.never ())
    in
    let randoms = Pipe.create_reader ~close_on_exception:false (fun writer ->
      Deferred.forever () (fun () ->
        Pipe.write writer (random ()));
      Deferred.never ())
    in
    Pipe.interleave [static; randoms]
    |> interpolate_pipe ~weighted_average ~num_steps
end

type t = Color.t array

let weights = Palette_weights.weights
let num_colors = Array.length weights

let create (c1, c2, c3) =
  Array.map weights ~f:(fun (w1, w2, w3) ->
    let open Color in
    scale c1 ~by:w1 + scale c2 ~by:w2 + scale c3 ~by:w3)

let create_graphics basis =
  create basis
  |> Array.map ~f:Color.graphics_color

let rgb = create Basis.rgb
let rgb_graphics = create_graphics Basis.rgb
