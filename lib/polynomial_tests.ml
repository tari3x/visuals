open Core

open Polynomial

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

let%expect_test _ =
  pow (var 1) 4 + pow (var 2) 4
  |> to_string
  |> print_endline;
  [%expect {| ((x)**4) + ((y)**4) |}]

 let%expect_test _ =
  all_monomials ~degree:2
  |> List.map ~f:to_string
  |> printf !"%{sexp:string list}";
  [%expect {|
    (1. y "(y)**2" x "(x) * (y)" "(x)**2") |}]

 let%expect_test _ =
  first_monomials 8
  |> List.map ~f:to_string
  |> printf !"%{sexp:string list}";
  [%expect {|
      (1. y x "(y)**2" "(x) * (y)" "(x)**2" "(y)**3" "(x) * ((y)**2)") |}]

let%expect_test _ =
  for i = 1 to 20 do
    printf "(%d %d) " i (List.length (all_monomials ~degree:i));
  done;
  [%expect {|
    (1 3) (2 6) (3 10) (4 15) (5 21) (6 28) (7 36) (8 45) (9 55) (10 66) (11 78) (12 91) (13 105) (14 120) (15 136) (16 153) (17 171) (18 190) (19 210) (20 231) |}]

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
      ((((-2.9999999999999929) + ((0.99999999999999645) * ((x) * (y)))) + ((1.9999999999999991) * ((x)**2))) + ((-4.) * (y))) + ((-3.5527136788005009e-15) * ((y)**2)) |}]
