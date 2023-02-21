(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Js_of_ocaml
open Js

let () =
  (* Choose 'matrix' (default) or 'array' *)
  Js.Unsafe.eval_string
    "math.config({ matrix: 'array' })"

module Matrix = struct
  type t = float js_array Js.t js_array Js.t

  let of_array a =
    Array.map a ~f:Js.array
    |> Js.array

  let to_array t =
    Js.to_array t
    |> Array.map ~f:Js.to_array
end

module Vector = struct
  type t = float js_array Js.t

  let of_array a =
    Js.array a

  let to_array t =
    Js.to_array t
end

let multiply m1 m2 =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "math.multiply")
    [| Js.Unsafe.inject m1; Js.Unsafe.inject m2 |]

let inv m =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "math.inv")
    [| Js.Unsafe.inject m |]

let lusolve m x =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "math.lusolve")
    [| Js.Unsafe.inject m; Js.Unsafe.inject x |]

let flatten m =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "math.flatten")
    [| Js.Unsafe.inject m |]
