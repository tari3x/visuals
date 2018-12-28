open Core
open Async

open Common

module V = Vector.Float
module Matrix = Maxima.Matrix

module T = struct
  include Maxima.Expr

  let pow t n =
    if n = 0 then const 1. else pow t n
end

include T

module B = Bigarray
module B1 = B.Array1
module B2 = B.Array2

let var_x = var 1
let var_y = var 2

let zero_line_between_two_points (x1, y1) (x2, y2) =
  if Float.(x2 = x1)
  then var_x - const x1
  else begin
    let slope = Float.((y2 - y1) / (x2 - x1)) in
    var_y - const y1 - const slope * (var_x - const x1)
  end

module E = struct
  type t =
  | Const of float
  | Var of int
  | Sum of t list
  | Product of t list
  | Pow of t * int
      [@@deriving variants]

  let ( * ) t1 t2 = product [t1; t2]
  (* let ( + ) t1 t2 = sum [t1; t2] *)

  let eval t values =
    let rec eval = function
      | Const f -> f
      | Var i -> List.nth_exn values Int.(i - 1)
      | Sum ts ->
        begin
          List.map ts ~f:eval
          |> List.fold ~init:0. ~f:Float.(+)
        end
      | Product ts ->
        begin
          List.map ts ~f:eval
          |> List.fold ~init:1. ~f:Float.( * )
        end
      | Pow (t, n) ->
        if n = 0 then 1.
        else Float.int_pow (eval t) n
    in
    eval t

  let rec to_maxima t =
    let open T in
    match t with
    | Const f -> const f
    | Var i -> var i
    | Sum ts -> sum (List.map ts ~f:to_maxima)
    | Product ts -> product (List.map ts ~f:to_maxima)
    | Pow (t, n) -> pow (to_maxima t) n

  (* 2D only *)
  let all_monomials ~degree:n =
    List.init Int.(n + 1) ~f:(fun i ->
      List.init Int.(n + 1) ~f:(fun j ->
        if Int.(i + j > n) then None
        else Some (pow (var 1) i *  pow (var 2) j))
      |> List.filter_opt)
    |> List.concat
end

let%expect_test _ =
  E.all_monomials ~degree:2
  |> List.map ~f:E.to_maxima
  |> printf !"%{sexp:t list}\n";
  [%expect {|
    ("(1.) * (1.)" "(1.) * ((y)**1)" "(1.) * ((y)**2)" "((x)**1) * (1.)"
     "((x)**1) * ((y)**1)" "((x)**2) * (1.)") |}]

let%expect_test _ =
  for i = 1 to 20 do
    printf "(%d %d) " i (List.length (E.all_monomials ~degree:i));
  done;
  [%expect {|
    (1 3) (2 6) (3 10) (4 15) (5 21) (6 28) (7 36) (8 45) (9 55) (10 66) (11 78) (12 91) (13 105) (14 120) (15 136) (16 153) (17 171) (18 190) (19 210) (20 231) |}]

module Datum = struct
  type t = V.t * float [@@deriving sexp]

  let weighted_average ((x1, y1), v1) ((x2, y2), v2) ~w =
    let x = weighted_average x1 x2 ~w in
    let y = weighted_average y1 y2 ~w in
    let v = weighted_average v1 v2 ~w in
    ((x, y), v)
end

(* Fortran layout is 1-based. *)
let bset b i j v =
  let open Int in
  b.{i + 1, j + 1} <- v

let bget b i j =
  let open Int in
  b.{i + 1, j + 1}

(*
  Lacaml doc:
  http://www.netlib.org/lapack/explore-html/d7/d3b/group__double_g_esolve_ga385713b8bcdf85663ff9a45926fac423.html#ga385713b8bcdf85663ff9a45926fac423
  https://mmottl.github.io/lacaml/api/lacaml/Lacaml/D/index.html
*)
let lagrange ~degree data =
  let monomials = E.all_monomials ~degree in
  let np = List.length data in
  let nm = List.length monomials in
  let points, values = List.unzip data in
  let m = B2.create B.Float64 B.Fortran_layout np nm in
  List.iteri points ~f:(fun i (x, y) ->
    List.iteri monomials ~f:(fun j p ->
      bset m i j (E.eval p [x; y])));
  let b = B2.create B.Float64 B.Fortran_layout (max nm np) 1 in
  List.iteri values ~f:(fun i value -> bset b i 0 value);
  let rank = Lacaml.D.gelsy m b in
  ignore rank;
  let coeffs = List.init nm ~f:(fun i -> bget b i 0) in
  List.map2_exn monomials coeffs ~f:(fun p c ->
    (* Don't round, very sensitive. *)
    (* let c = Float.round_decimal c ~decimal_digits:13 in*)
    E.to_maxima p * const c)
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
    (-3.463542432163259E-15*y^2)+1.000000000000005*x*y-3.999999999999988*y+2.0*x^2-7.017996670400557E-15*x-3.00000000000001 |}]
