(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal
module Config = Config.Skin
module Shape = Shapes.Elt

type t

val create : config:Config.t -> sound:Sound.t -> Shapes.t -> t
val set_config : t -> Config.t -> unit

(* CR avatar: this needs to take symmetric diff and only create new ones. *)
val set_shapes : t -> Shapes.t -> unit
val human_touch : t -> Shape.t -> Color.t -> unit
val render : t -> perspective:Matrix.t -> pixi:Pixi.t -> unit
