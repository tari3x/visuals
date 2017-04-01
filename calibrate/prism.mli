(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Geometry
open Dom_wrappers

(* A collection of surfaces to project onto, each defined by 4 points.
*)

module Quad : sig
  type t

  val create : Vector.t -> Vector.t -> Vector.t -> Vector.t -> t

  val draw : t -> ctx:Ctx.t -> color:Color.t -> unit

  val contains : t -> Vector.t -> bool
end

module Surface : sig
  type t

  val create : canvas:Quad.t -> camera:Quad.t -> t
end

type t

val create : Surface.t list -> t

val camera_to_canvas : t -> Vector.t -> Vector.t option

val draw : t -> ctx:Ctx.t -> unit

