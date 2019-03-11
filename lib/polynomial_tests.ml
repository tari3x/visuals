open Core

open Polynomial

let x = var 1
let y = var 2

let%expect_test _ =
  (const 3.) * ((const 1.) + (const (2.) * (pow (y) 2)))
  |> to_string
  |> print_endline;
  [%expect {| (3.) + ((6.) * ((y)**2)) |}]

let%expect_test _ =
  (x) + (x)
  |> to_string
  |> print_endline;
  [%expect {| (2.) * (x) |}]

let%expect_test _ =
  pow (x) 4 + pow (y) 4
  |> to_string
  |> print_endline;
  [%expect {| ((x)**4) + ((y)**4) |}]

let%expect_test _ =
  Basis.mono ~degree:2
  |> List.map ~f:to_string
  |> printf !"%{sexp:string list}";
  [%expect {|
      (1. y "(y)**2" x "(x) * (y)" "(x)**2") |}]

let unit_domain = ((0., 0.), (1., 1.))

let%expect_test _ =
  Basis.bernstein ~degree:1 ~domain:unit_domain
  |> List.map ~f:to_string
  |> printf !"%{sexp:string list}";
  [%expect {|
      (y x "((1.) + ((-1.) * (x))) + ((-1.) * (y))") |}]

let%expect_test _ =
  first_monomials 8
  |> List.map ~f:to_string
  |> printf !"%{sexp:string list}";
  [%expect {|
      (1. y x "(y)**2" "(x) * (y)" "(x)**2" "(y)**3" "(x) * ((y)**2)") |}]

let%expect_test _ =
  subst (const 1. + pow x 2 + x) ~var:(Var.create 1) ~by:y
  |> to_string
  |> print_endline;
  [%expect {|
      ((1.) + (y)) + ((y)**2) |}]

let%expect_test _ =
  for i = 1 to 20
  do begin
    let num_mono =
      List.length (Basis.mono ~degree:i)
    in
    let num_bernstein =
      List.length (Basis.bernstein ~degree:i ~domain:unit_domain)
    in
    assert Int.(num_mono = num_bernstein);
    printf "(%d %d) " i num_mono;
  end
  done;
  [%expect {|
      (1 3) (2 6) (3 10) (4 15) (5 21) (6 28) (7 36) (8 45) (9 55) (10 66) (11 78) (12 91) (13 105) (14 120) (15 136) (16 153) (17 171) (18 190) (19 210) (20 231) |}]

(* "A Simple Expression for Multivariate Lagrange Interpolation." *)
let lagrange_data =
  [ (0, 1), -7
  ; (2, 1), 3
  ; (1, 3), -10
  ; (-2, -1), 11
  ; (-3, 2), 1
  ; (-1, 2), -11
  ]
  |> List.map ~f:(fun ((x, y), z) -> ((float x, float y), float z))

let%expect_test _ =
  let t = Lagrange.simple lagrange_data ~basis:(Basis.mono ~degree:2) in
  printf !"%s\n" (to_string t);
  [%expect {|
      ((((-2.9999999999999929) + ((0.99999999999999645) * ((x) * (y)))) + ((1.9999999999999991) * ((x)**2))) + ((-4.) * (y))) + ((-3.5527136788005009e-15) * ((y)**2)) |}]

let%expect_test _ =
  let t =
    Lagrange.simple
      lagrange_data
      ~basis:(Basis.bernstein ~degree:2 ~domain:unit_domain)
  in
  printf !"%s\n" (to_string t);
  [%expect {|
      (((((-3.0000000000000213) + ((-1.0658141036401503e-14) * (x))) + ((1.0000000000000142) * ((x) * (y)))) + ((2.0000000000000022) * ((x)**2))) + ((-3.9999999999999716) * (y))) + ((-1.0658141036401503e-14) * ((y)**2)) |}]
