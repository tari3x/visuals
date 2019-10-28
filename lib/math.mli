(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Js_of_ocaml
open Js

module Matrix : sig
  type t = float js_array Js.t js_array Js.t

  val of_array : float array array -> t
  val to_array : t -> float array array
end

module Vector : sig
  type t = float js_array Js.t

  val of_array : float array -> t
  val to_array : t -> float array
end

val multiply : Matrix.t -> Matrix.t -> Matrix.t

val inv : Matrix.t -> Matrix.t

val lusolve : Matrix.t -> Vector.t -> Matrix.t

val flatten : Matrix.t -> Vector.t
