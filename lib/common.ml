open Core
open Async

let () = Random.init 150

let weighted_average x1 x2 ~w =
  let open Float in
  (1. - w) * x1 + w * x2

let interpolate ~weighted_average ~num_steps xs =
  let rec loop = function
    | [] -> []
    | [x] -> [x]
    | x1 :: x2 :: xs ->
      List.init num_steps ~f:(fun n ->
        let w = float n /. float num_steps in
        weighted_average ~w x1 x2)
      @ loop (x2 :: xs)
  in
  loop xs

let fold_map_deferred xs ~init ~f =
  let rec loop y = function
    | [] -> return [y]
    | x :: xs ->
      let%bind y' = f y x in
      let%bind ys = loop y' xs in
      return (y :: ys)
  in
  loop init xs

let rec intercalate xs ys =
  match xs, ys with
  | [], ys -> ys
  | xs, [] -> xs
  | (x :: xs), (y :: ys) ->
    x :: y :: intercalate xs ys

module Float = struct
  include Float

  let sum =
    List.fold ~init:0. ~f:(+)

  let product =
    List.fold ~init:1. ~f:( * )
end

let debug ?(should_debug = false) a =
  ksprintf (fun s -> if should_debug then print_endline s) a
