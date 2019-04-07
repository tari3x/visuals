(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Core
open Common

module type Num = sig
  type t [@@deriving sexp]
  include Comparable with type t := t
  val mid : t -> t -> t
  val double: t -> t
end

module type S = sig
  type t

  val search
    :  bounds:(t * t)
    -> test:(t -> int)
    -> ?start:t
    -> unit
    -> t
end

module Make(N : Num) : S with type t = N.t = struct
  type t = N.t [@@deriving sexp]

  let search ~bounds:(x1, x2) ~test ?start () =
    let num_iter = ref 0 in
    let rec between x1 x2 =
      incr num_iter;
      if !num_iter > 10000
      then failwithf !"%{Sexp}"
        [%message "binary search stuck" (x1 : t) (x2 : t)] ();
      let x_mid = N.mid x1 x2 in
      if N.(x_mid = x1 || x_mid = x2)
      then begin
        if test x2 <= 0 then x2 else x1
      end
      else begin
        let c = test x_mid in
        if      c > 0 then between x1 x_mid
        else if c < 0 then between x_mid x2
        else x_mid
      end
    in
    let above x1 =
      let rec loop x =
        let x = N.double x in
        if N.(x >= x2) then between x1 x2
        else begin
          let c = test x in
          if      c > 0 then between x1 x
          else if c < 0 then loop x
          else x
        end
      in
      loop x1
    in
    let with_start x =
      let c = test x in
      if      c > 0 then between x1 x
      else if c < 0 then above x
      else x
    in
    if      test x2 <= 0 then x2
    else if test x1 >= 0 then x1
    else match start with
    | Some x_mid -> with_start x_mid
    | None -> between x1 x2
end

module Float : S with type t = float =
  Make(struct
    include Float

    let mid t1 t2 =
      (t1 + t2) / 2.

    let double t =
      t * 2.
  end)

module Int : S with type t = int
  = Make(struct
    include Int

    let mid t1 t2 =
      (t1 + t2) / 2

    let double t =
      t * 2
  end)

let%expect_test _ =
  Float.search ()
    ~bounds:Core.Float.(- max_finite_value, max_finite_value)
    ~test:(fun x -> Core.Float.compare x 2.456)
  |> printf "%f";
   [%expect {| 2.456000 |}]

let%expect_test _ =
  Int.search ()
    ~bounds:(- 100, 100)
    ~test:(fun x -> Core.Int.compare x 23)
  |> printf "%d";
   [%expect {| 23 |}]

let%expect_test _ =
  Int.search ()
    ~bounds:(- 100, 100)
    ~test:(fun x -> Core.Int.compare x 100)
  |> printf "%d";
   [%expect {| 100 |}]
