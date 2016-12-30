(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Js

module Matrix : sig
  type t = float js_array Js.t js_array Js.t
end

val multiply : Matrix.t -> Matrix.t -> Matrix.t

val inv : Matrix.t -> Matrix.t
