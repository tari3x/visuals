(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Core
open Std_internal

(* Mutated in place. *)
module Elt : sig
  module Id : Identifiable.S

  type t [@@deriving sexp]

  val id : t -> Id.t
  val centre : t -> V.t

  val render
    :  t
    -> perspective:Matrix.t
    -> pixi:Pixi.t
    -> color:Color.t
    -> unit

  val set_transform : t -> Matrix.t -> unit
end

type t [@@deriving sexp]

val create
  :  corners:Prism.Quad.t
  -> step:float
  -> line_width:float
  -> Shape.t list
  -> t

val create_with_pixi
  :  pixi:Pixi.t
  -> step:float
  -> line_width:float
  -> Shape.t list
  -> t

val elts : t -> Elt.t Elt.Id.Map.t
val corners : t -> Prism.Quad.t
val step : t -> float

(** Both must be positive *)
val grid_exn : pixi:Pixi.t -> rows:int -> cols:int -> t

val hex_wire_exn : pixi:Pixi.t -> r1_mult:float -> t
val hex_tile_exn : pixi:Pixi.t -> r1_mult:float -> t
val hex_bone_exn : pixi:Pixi.t -> r1_mult:float -> t
val set_transform : t -> Matrix.t -> unit

(* CR avatar: short this crazy out *)
val update : t -> t -> t
