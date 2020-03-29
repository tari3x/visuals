(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

module Ctl : sig
  type t [@@deriving sexp]

  val spot : t
  val rain_control : t

  (* must be not empty *)
  val set_shapes_exn : Shape.t list -> t
end

type t

(* CR-someday: sound must be made optional if you want to display grid on the
   client .*)
val create
  :  config:Config.Skin.t
  -> pixi:Pixi.t
  -> sound:Sound.t
  -> shapes:Shapes.t
  -> ?real_corners:Prism.Quad.t
  -> unit
  -> t

val ctl : t -> Ctl.t Box.t -> unit
val render : t -> unit
