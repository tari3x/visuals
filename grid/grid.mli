(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

module Ctl : sig
  type t =
  | Spot
  | Rain_control
      [@@deriving sexp]
end

type t

val create
  :  ctx:Ctx.t
  (* CR-someday: sound must be made optional if you want to display grid on the
     client .*)
  -> sound:Sound.t
  -> rows:int
  -> cols:int
  -> ?corners:Prism.Quad.t
  -> color:Color.t
  -> unit
  -> t

val ctl : t -> Ctl.t Box.t -> unit

val render : t -> unit
