(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

module Shape : sig
  include module type of Shape

  val centre : t -> V.t

  val render : t -> perspective:Matrix.t -> ctx:Ctx.t -> color:Color.t -> unit
end

type t

val shapes : t -> Shape.t list
val corners : t -> Prism.Quad.t

(** must be not empty *)
val create_exn : corners:Prism.Quad.t -> Shape.t list -> t

(** both must be positive *)
val grid_exn : ctx:Ctx.t -> rows:int -> cols:int -> t

val hex_wire_exn : ctx:Ctx.t -> t
val hex_tile_exn : ctx:Ctx.t -> t
