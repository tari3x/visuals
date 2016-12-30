(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Js

module Matrix = struct
  type t = float js_array Js.t js_array Js.t
end

let multiply m1 m2 =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "math.multiply")
    [| Js.Unsafe.inject m1; Js.Unsafe.inject m2 |]

let inv m =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "math.inv")
    [| Js.Unsafe.inject m |]
