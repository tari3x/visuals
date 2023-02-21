open Core
open Async
include Int.Replace_polymorphic_compare

let () = Random.init 150

module type Ring = sig
  type t [@@deriving sexp, compare, equal]

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
end

let weighted_average x1 x2 ~w =
  let open Float in
  ((1. - w) * x1) + (w * x2)
;;

let interpolate ~weighted_average ~num_steps xs =
  let rec loop = function
    | [] -> []
    | [ x ] -> [ x ]
    | x1 :: x2 :: xs ->
      List.init num_steps ~f:(fun n ->
          let w = float n /. float num_steps in
          weighted_average ~w x1 x2)
      @ loop (x2 :: xs)
  in
  loop xs
;;

(* CR-someday: surely should work simpler? *)
let interpolate_pipe ~weighted_average ~num_steps pipe =
  let reader, writer = Pipe.create () in
  let finish () =
    Core.print_s [%message "FINISH"];
    Pipe.close writer;
    return ()
  in
  let rec loop x1 =
    match%bind Pipe.read pipe with
    | `Eof -> finish ()
    | `Ok x2 ->
      let%bind () =
        interpolate ~weighted_average ~num_steps [ x1; x2 ]
        |> Deferred.List.iter ~f:(Pipe.write writer)
      in
      loop x2
  in
  let start () =
    match%bind Pipe.read pipe with
    | `Eof -> finish ()
    | `Ok x -> loop x
  in
  don't_wait_for (start ());
  reader
;;

module Float = struct
  include Float

  let sum = List.fold ~init:0. ~f:( + )
  let product = List.fold ~init:1. ~f:( * )
  let average_exn xs = sum xs / float (List.length xs)
  let sign_val t = robust_sign t |> Sign.to_float
end

module Int = struct
  include Int

  let sum = List.fold ~init:0 ~f:( + )
  let product = List.fold ~init:1 ~f:( * )

  let rec factorial n =
    if n < 0
    then failwith "factorial argument must not be negative"
    else if n = 0
    then 1
    else n * factorial (n - 1)
  ;;

  let%expect_test _ =
    printf "%d" (factorial 10);
    [%expect {|
          3628800 |}]
  ;;
end

let debug ~enabled a =
  ksprintf (fun s -> if enabled then Core.printf "%s\n%!" s) a
;;

let debug_s ~enabled s = if enabled then Core.print_s s

module List = struct
  include List

  let take t ~n = take t n

  let rec product = function
    | [] -> []
    | [ t ] -> map t ~f:return
    | t :: ts ->
      List.cartesian_product t (product ts)
      |> List.map ~f:(fun (x, t) -> x :: t)
  ;;

  let%expect_test _ =
    product [ [ "x1"; "x2" ]; [ "y1" ]; [ "z1"; "z2"; "z3" ] ]
    |> printf !"%{sexp:string list list}\n";
    [%expect
      {| ((x1 y1 z1) (x1 y1 z2) (x1 y1 z3) (x2 y1 z1) (x2 y1 z2) (x2 y1 z3)) |}]
  ;;

  let scan xs ~init ~f =
    let f x y =
      let r = f x y in
      r, r
    in
    folding_map xs ~init ~f
  ;;

  let scan_deferred xs ~init ~f =
    let open Deferred.Let_syntax in
    let rec loop y = function
      | [] -> return []
      | x :: xs ->
        let%bind y = f y x in
        let%bind ys = loop y xs in
        return (y :: ys)
    in
    loop init xs
  ;;

  let rec intercalate xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, y :: ys -> x :: y :: intercalate xs ys
  ;;
end
