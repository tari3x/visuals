open Core
open Async

open Common

module E = Maxima.Expr
module V = Vector.Float
module Matrix = Maxima.Matrix

type t =
| Const of float
| Var of int
| Sum of t list
| Product of t list
| Pow of t * int
    [@@deriving variants]

let _descend t ~f =
  match t with
  | Const x -> Const x
  | Var n -> Var n
  | Sum ts -> Sum (List.map ts ~f)
  | Product ts -> Product (List.map ts ~f)
  | Pow (t, n) -> Pow (f t, n)

let rec to_maxima t =
  match t with
  | Const f -> E.const f
  | Var i -> E.var i
  | Sum ts -> E.sum (List.map ts ~f:to_maxima)
  | Product ts -> E.product (List.map ts ~f:to_maxima)
  | Pow (t, n) -> E.pow (to_maxima t) n

let split_const =
  List.partition_map ~f:(function
  | Const x -> `Fst x
  | t -> `Snd t)

let sum ts =
  let sum = function
    | [] -> const 0.
    | [x] -> x
    | ts -> sum ts
  in
  let cs, ts = split_const ts in
  let c = Float.sum cs in
  if Float.(c = 0.) then sum ts
  else sum (const c :: ts)

let product ts =
  let product = function
    | [] -> const 1.
    | [x] -> x
    | ts -> product ts
  in
  let cs, ts = split_const ts in
  let c = Float.product cs in
  if Float.(c = 1.) then product ts
  else product (const c :: ts)

let pow t n =
  if n = 0 then const 1.
  else if n = 1 then t
  else pow t n

let to_string t =
  to_maxima t |> E.to_string

let to_gnuplot t =
  to_maxima t
  |> E.to_gnuplot

let ( * ) t1 t2 =
  product [t1; t2]

let ( + ) t1 t2 =
  sum [t1; t2]

let scale t x =
  product [ const x; t ]

let ( - ) t1 t2 =
  sum [t1; scale t2 (-1.)]

let var_x = var 1
let var_y = var 2

let zero_line_between_two_points (x1, y1) (x2, y2) =
  if Float.(x2 = x1)
  then var_x - const x1
  else begin
    let slope = Float.((y2 - y1) / (x2 - x1)) in
    var_y - const y1 - const slope * (var_x - const x1)
  end

let ( * ) t1 t2 = product [t1; t2]
  (* let ( + ) t1 t2 = sum [t1; t2] *)

let eval t values =
  let rec eval = function
    | Const f -> f
    | Var i -> List.nth_exn values Int.(i - 1)
    | Sum ts -> List.map ts ~f:eval |> Float. sum
    | Product ts -> List.map ts ~f:eval |> Float.product
    | Pow (t, n) ->
      if n = 0 then 1.
      else Float.int_pow (eval t) n
  in
  eval t

(* 2D only *)
let all_monomials ~degree:n =
  List.init Int.(n + 1) ~f:(fun i ->
    List.init Int.(n + 1) ~f:(fun j ->
      if Int.(i + j > n) then None
      else Some (pow (var 1) i *  pow (var 2) j))
    |> List.filter_opt)
  |> List.concat

let%expect_test _ =
  all_monomials ~degree:2
  |> List.map ~f:to_string
  |> printf !"%{sexp:string list}\n";
  [%expect {|
    (1. y "(y)**2" x "(x) * (y)" "(x)**2") |}]

let%expect_test _ =
  for i = 1 to 20 do
    printf "(%d %d) " i (List.length (all_monomials ~degree:i));
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

module Data = struct
  type t = Datum.t list
end

let error t data =
  let open Float in
  List.map data ~f:(fun ((x, y), value) ->
    abs (eval t [x; y] - value))
  |> Float.sum

module B = Bigarray
module B1 = B.Array1
module B2 = B.Array2

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
  let monomials = all_monomials ~degree in
  let np = List.length data in
  let nm = List.length monomials in
  let points, values = List.unzip data in
  let m = B2.create B.Float64 B.Fortran_layout np nm in
  List.iteri points ~f:(fun i (x, y) ->
    List.iteri monomials ~f:(fun j p ->
      bset m i j (eval p [x; y])));
  let b = B2.create B.Float64 B.Fortran_layout (max nm np) 1 in
  List.iteri values ~f:(fun i value -> bset b i 0 value);
  (* gelsy gives smaller error. *)
  let rank = Lacaml.D.gelsy m b in
  ignore rank;
  (*
  Lacaml.D.gesv m b;
  *)
  let coeffs = List.init nm ~f:(fun i -> bget b i 0) in
  List.map2_exn monomials coeffs ~f:(fun p c ->
    (* Don't round, very sensitive. *)
    (* let c = Float.round_decimal c ~decimal_digits:13 in*)
    const c * p)
  |> sum
  |> return

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
    (((((-3.0000000000000098) + ((-3.9999999999999876) * (y))) + ((-3.4635424321632592e-15) * ((y)**2))) + ((-7.0179966704005574e-15) * (x))) + ((1.0000000000000047) * ((x) * (y)))) + ((2.0000000000000004) * ((x)**2)) |}]
