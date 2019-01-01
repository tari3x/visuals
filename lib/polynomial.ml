open Core
open Async

open Common

module E = Maxima.Expr
module V = Vector.Float
module Matrix = Maxima.Matrix

let _debug a = debug ~enabled:true a

module Var = struct
  include String

  let create n =
    E.(var n |> to_string)
end

module Mono = struct
  module T = struct
    type t = (Var.t * int) list
    [@@deriving sexp, compare]
  end

  include T

  let one : t = []

  let is_one = List.is_empty

  let var n : t =
    [Var.create n, 1]

  let ( * ) (t1 : t) (t2 : t) : t =
    Var.Map.of_alist_multi (t1 @ t2)
    |> Map.to_alist
    |> List.map ~f:(fun (var, powers) ->
      var, Int.sum powers)

  let to_maxima t =
    List.map t ~f:(fun (v, n) ->
      E.(pow (of_string v) n))
    |> E.product

  let eval (t : t) (values : float Var.Map.t) =
    List.map t ~f:(fun (v, n) ->
      Float.int_pow (Map.find_exn values v) n)
    |> Float.product

  include Comparable.Make(T)
end

type t = (Mono.t * float) list

let zero = []

let const x =
  [Mono.one, x]

let var n =
  [Mono.var n, 1.]

let group_sum t =
  Mono.Map.of_alist_multi t
  |> Map.to_alist
  |> List.filter_map ~f:(fun (mono, coeffs) ->
    let c = Float.sum coeffs in
    if Float.(c = 0.) then None
    else Some (mono, c))

let ( + ) (t1 : t) (t2 : t) : t =
  group_sum (t1 @ t2)

let ( * ) (t1 : t) (t2 : t) : t =
  List.cartesian_product t1 t2
  |> List.map ~f:(fun ((m1, c1), (m2, c2)) ->
    (Mono.(m1 * m2), Float.(c1 * c2)))
  |> group_sum

let product = function
  | [] -> (const 1.)
  | ts -> List.reduce_exn ts ~f:( * )

let sum = function
  | [] -> zero
  | ts -> List.reduce_exn ts ~f:(+)

(* CR-someday: make efficient. *)
let pow t n =
  List.init n ~f:(fun _ -> t)
  |> product

let scale t x =
  product [ const x; t ]

let ( - ) t1 t2 =
  sum [t1; scale t2 (-1.)]

let var_x = var 1
let var_y = var 2

let to_maxima (t : t) =
  List.map t ~f:(fun (m, c) ->
    if Mono.is_one m then E.const c
    else begin
      let m = Mono.to_maxima m in
      if Float.(c = 1.) then m
      else E.(const c * m)
    end)
  |> E.sum

let to_string t =
  to_maxima t |> E.to_string

let to_gnuplot t =
  to_maxima t
  |> E.to_gnuplot

let%expect_test _ =
  (const 3.) * ((const 1.) + (const (2.) * (pow (var 2) 2)))
  |> to_string
  |> print_endline;
  [%expect {| (3.) + ((6.) * ((y)**2)) |}]

let%expect_test _ =
  (var 1) + (var 1)
  |> to_string
  |> print_endline;
  [%expect {| (2.) * (x) |}]

let zero_line_between_two_points (x1, y1) (x2, y2) =
  if Float.(x2 = x1)
  then var_x - const x1
  else begin
    let slope = Float.((y2 - y1) / (x2 - x1)) in
    var_y - const y1 - const slope * (var_x - const x1)
  end

let eval (t : t) (values : float list) =
  let values =
    List.mapi values ~f:(fun i v -> Var.create Int.(i + 1), v)
    |> Var.Map.of_alist_exn
  in
  List.map t ~f:(fun (m, c) ->
    Float.(c * Mono.eval m values))
  |> Float.sum

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

  (*
module Lagrange_state = struct
  type t =
    { ps : string list
    ; qs : string list
    ; vs : V.t list
    ; values : float list list
    ; v : V.t
    } [@@deriving sexp]

  let create ps qs vs v =
    let values =
      List.map ps ~f:(fun p ->
        List.map vs ~f:(fun (x, y) ->
          eval p [x; y]))
    in
    let ps = List.map ps ~f:to_string in
    let qs = List.map qs ~f:to_string in
    { ps; qs; vs; values; v }
end
  *)

let lagrange ~degree data =
  let num_points = List.length data in
  let points_done = ref [] in
  let add_point ps qs (x, y) =
    (*
       Lagrange_state.create ps qs !points_done (x, y)
       |> debug !"%{sexp:Lagrange_state.t}";
    *)
    let eval q = eval q [x; y] in
    let qs =
      List.sort qs ~compare:(fun q1 q2 ->
        Float.compare (Float.abs (eval q1)) (Float.abs (eval q2)))
      |> List.rev
    in
    match qs with
    | [] -> failwithf "degree %d is too low for %d points" degree num_points ()
    | q :: qs ->
      let q_val = eval q in
      if Float.(q_val = 0.) then failwithf "No unique interpolation" ();
      let new_p = scale q (1. /. q_val) in
      let adjust p = p - scale new_p (eval p) in
      let ps = List.map ps ~f:adjust in
      let qs = List.map qs ~f:adjust in
      (new_p :: ps), qs
  in
  let rec loop ps qs = function
    | [] -> List.rev ps
    | v :: vs ->
      let ps, qs = add_point ps qs v in
      points_done := v :: !points_done;
      loop ps qs vs
  in
  let qs = List.take (all_monomials ~degree) num_points in
  let points, values = List.unzip data in
  let ps = loop [] qs points in
  List.map2_exn ps values ~f:scale
  |> sum

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
  let t = lagrange data ~degree:2 in
  printf !"%s\n" (to_string t);
  [%expect {|
      ((((-2.9999999999999929) + ((0.99999999999999289) * ((x) * (y)))) + ((2.) * ((x)**2))) + ((-4.) * (y))) + ((-3.5527136788005009e-15) * ((y)**2)) |}]
