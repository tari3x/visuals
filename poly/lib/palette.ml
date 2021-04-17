open Core
open Async
open Common
open Color

module Basis = struct
  type t = Color.t * Color.t * Color.t [@@deriving sexp]

  let weighted_average ~w (r1, g1, b1) (r2, g2, b2) =
    let r = Color.weighted_average r1 r2 ~w in
    let g = Color.weighted_average g1 g2 ~w in
    let b = Color.weighted_average b1 b2 ~w in
    r, g, b
  ;;

  let rgb = red, green, blue

  let split_unit () =
    let open Float in
    let x1 = Random.float 1. in
    let x2 = Random.float 1. in
    let x3 = Random.float 1. in
    let total = x1 + x2 + x3 in
    let x1 = x1 / total in
    let x2 = x2 / total in
    let x3 = x3 / total in
    x1, x2, x3
  ;;

  let random () =
    let r1, r2, r3 = split_unit () in
    let g1, g2, g3 = split_unit () in
    let b1, b2, b3 = split_unit () in
    Color.rgb r1 g1 b1, Color.rgb r2 g2 b2, Color.rgb r3 g3 b3
  ;;

  let pipe config =
    let num_steps = Float.(100. / Config.speed config) |> Int.of_float in
    let ts =
      [| red, green, blue
       ; red, blue, green
       ; green, red, blue
       ; green, blue, red
       ; blue, red, green
       ; blue, green, red
      |]
    in
    let current = ref 0 in
    let static =
      Pipe.create_reader ~close_on_exception:false (fun writer ->
          Deferred.forever () (fun () ->
              incr current;
              Pipe.write writer ts.(!current % Array.length ts));
          Deferred.never ())
    in
    let randoms =
      Pipe.create_reader ~close_on_exception:false (fun writer ->
          Deferred.forever () (fun () -> Pipe.write writer (random ()));
          Deferred.never ())
    in
    Pipe.interleave [ static; randoms ]
    |> interpolate_pipe ~weighted_average ~num_steps
  ;;
end

type t = Color.t array

let weights = Palette_weights.weights
let num_colors = Array.length weights

let create (c1, c2, c3) =
  Array.map weights ~f:(fun (w1, w2, w3) ->
      let open Color in
      scale c1 ~by:w1 + scale c2 ~by:w2 + scale c3 ~by:w3)
;;

let create_graphics basis = create basis |> Array.map ~f:Color.graphics
let rgb = create Basis.rgb
let rgb_graphics = create_graphics Basis.rgb
