open Core
open Common

module type Num = sig
  type t [@@deriving sexp]
  include Comparable with type t := t
  val mid : t -> t -> t
end

module type S = sig
  type t

  val search
    :  start:(t * t)
    -> test:(t -> int)
    -> t
end

module Make(N : Num) : S with type t = N.t = struct
  type t = N.t [@@deriving sexp]

  let search ~start:(x1, x2) ~test =
    let num_iter = ref 0 in
    let rec loop x1 x2 =
      incr num_iter;
      if !num_iter > 10000
      then failwithf !"%{Sexp}"
        [%message "binary search stuck" (x1 : t) (x2 : t)] ();
      let x_mid = N.mid x1 x2 in
      if N.(x_mid = x1 || x_mid = x2) then x1
      else begin
        let c = test x_mid in
        if      c < 0 then loop x1 x_mid
        else if c > 0 then loop x_mid x2
        else x_mid
      end
    in
    if      test x1 = 0 then x1
    else if test x2 = 0 then x2
    else loop x1 x2
end

module Float : S with type t = float =
  Make(struct
    include Float

    let mid t1 t2 =
      (t1 + t2) / 2.
  end)

module Int : S with type t = int
  = Make(struct
    include Int

    let mid t1 t2 =
      (t1 + t2) / 2
  end)

let%expect_test _ =
  Float.search
    ~start:Core.Float.(- max_finite_value, max_finite_value)
    ~test:(fun x -> Core.Float.compare 2.456 x)
  |> printf "%f";
   [%expect {| 2.456000 |}]

let%expect_test _ =
  Int.search
    ~start:(- 100, 100)
    ~test:(fun x -> Core.Int.compare 23 x)
  |> printf "%d";
   [%expect {| 23 |}]

let%expect_test _ =
  Int.search
    ~start:(- 100, 100)
    ~test:(fun x -> Core.Int.compare 100 x)
  |> printf "%d";
   [%expect {| 100 |}]
