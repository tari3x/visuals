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
  | Set_shapes of Shape.t list
      [@@deriving sexp]
end

type t

module Shapes : sig
  type t =
  | Grid of { rows: int; cols : int }
  | Set of Shape.t list
end

val create
  :  config:Config.t
  -> ctx:Ctx.t
  (* CR-someday: sound must be made optional if you want to display grid on the
     client .*)
  -> sound:Sound.t
  -> shapes:Shapes.t
  -> ?native_corners:Prism.Quad.t
  -> ?real_corners:Prism.Quad.t
  -> base_color:Color.t
  -> unit
  -> t

val ctl : t -> Ctl.t Box.t -> unit

val render : t -> unit
