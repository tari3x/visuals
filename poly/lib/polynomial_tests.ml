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
      (500. y "(y)**2" x "(x) * (y)" "(x)**2") |}]

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
      (((((-5.67758042615077) + ((-1.5563804567322999) * (x))) + ((2.4316858605132694) * ((x) * (y)))) + ((2.5876228146032636) * ((x)**2))) + ((-0.11193896868456621) * (y))) + ((-1.0947932966972811) * ((y)**2)) |}]

let%expect_test _ =
  let t =
    Lagrange.simple
      lagrange_data
      ~basis:(Basis.bernstein ~degree:2 ~domain:unit_domain)
  in
  printf !"%s\n" (to_string t);
  [%expect {|
      (((((-2.9999999999999645) + ((1.4210854715202004e-14) * (x))) + ((0.99999999999999645) * ((x) * (y)))) + ((2.0000000000000031) * ((x)**2))) + ((-4.00000000000005) * (y))) + ((1.7763568394002505e-14) * ((y)**2)) |}]
