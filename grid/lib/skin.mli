(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

module type Elt = sig
  module Id : Id

  type t

  val id : t -> Id.t
  val offset : t -> t -> Vector.t
  val touch : t -> Color_flow.t -> unit
end

module Make(Elt : Elt) : sig
  module Elts : sig
    type t

    (** Must have at least two elements *)
    val create_exn : Elt.t list -> t
  end

  type t

  val start
    :  config:Config.t
    -> sound:Sound.t
    -> Elts.t
    -> t

  (* CR-someday: allow to [stop] the bot such that a new one can be created
     with new elements. *)
  val set_elts : t -> Elts.t -> unit

  val human_touch : t -> Elt.t -> Color.t -> unit
end
