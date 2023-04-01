(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal
module Config = Config.Spark

module Ctl : sig
  type t = private
    | Spot
    | Rain_control
    | Set_config of Config.t
  [@@deriving sexp, variants]
end

type t

(* CR-someday: sound must be made optional if you want to display grid on the
   client .*)
val create
  :  config:Config.t
  -> pixi:Pixi.t
  -> sound:Sound.t
  -> ?real_corners:Prism.Quad.t
  -> unit
  -> t

val ctl : t -> Ctl.t Box.t -> unit
val render : t -> unit
