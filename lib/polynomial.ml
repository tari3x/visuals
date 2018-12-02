open Core
open Async

module Matrix = Maxima.Matrix
include Maxima.Expr

let var_x = var 1
let var_y = var 2

let exp t n =
  if n = 0 then const 1. else exp t n

let zero_line_between_two_points (x1, y1) (x2, y2) =
  if Float.(x2 = x1)
  then var_x - const x1
  else begin
    let slope = Float.((y2 - y1) / (x2 - x1)) in
    var_y - const y1 - const slope * (var_x - const x1)
  end

(* 2D only *)
let all ~degree:n =
  List.init Int.(n + 1) ~f:(fun i ->
    List.init Int.(n + 1) ~f:(fun j ->
      if Int.(i + j > n) then None
      else Some (exp var_x i *  exp var_y j))
    |> List.filter_opt)
  |> List.concat

let%expect_test _ =
  printf !"%{sexp:t list}\n" (all ~degree:2);
  [%expect {|
    ("(1.) * (1.)" "(1.) * ((y)**1)" "(1.) * ((y)**2)" "((x)**1) * (1.)"
     "((x)**1) * ((y)**1)" "((x)**2) * (1.)") |}]

let%expect_test _ =
  for i = 1 to 20 do
    printf "%d " (List.length (all ~degree:i));
  done;
  [%expect {|
    3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210 231 |}]


let lagrange ~degree data =
  let num_points = List.length data in
  let monomials = all ~degree in
  [%test_result: int] ((List.length monomials)) ~expect:num_points;
  let points, values = List.unzip data in
  let m = List.map points ~f:(fun (x, y) ->
    List.map monomials ~f:(fun p -> (ev p [const x; const y])))
  in
  let ms =
    List.init num_points ~f:(fun i ->
      List.take m i @ [ monomials ] @ List.drop m Int.(i + 1))
  in
  let det m = Matrix.det (Matrix.create m) in
  let det_m = det m in
  let det_ms = List.map ~f:det ms in
  List.map2_exn det_ms values ~f:(fun d value ->
    if Float.(value = 0.)
    then None
    else Some (const value * d / det_m))
  |> List.filter_opt
  |> sum
  |> expand
  |> eval

let%expect_test _ =
  let data =
    [ (0, 1), -7
    ; (2, 1), 3
    ; (1, 3), -10
    ; (-2, -1), 11
    ; (-3, 2), 1
    ; (-1, 2), -11
    ]
    |> List.map ~f:(fun ((x, y), z) -> ((float x, float y), float z))
  in
  let%bind t = lagrange data ~degree:2 in
  printf !"%s\n" (to_string t);
  [%expect {|
    x*y-4*y+2*x^2-3 |}]
