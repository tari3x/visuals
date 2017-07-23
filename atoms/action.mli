(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Geometry

module Kind : sig
  type t = [ `down | `up | `move ]
  [@@deriving compare]

  val to_string : t -> string

  val equal : t -> t -> bool
end

module Button : sig
  type t = [ `left | `right | `middle | `touch | `none ]

  val to_string : t -> string
end

module Pointer_id : sig
  include Identifiable.S
  val create : int -> t
end

(* Either a touch or a mouse. *)
module Pointer : sig
  type t =
    { id : Pointer_id.t
    ; position : Vector.t
    }

  val id : t -> Pointer_id.t
end

type t =
  { kind : Kind.t
  ; changed_touches : Pointer.t list
  ; button : Button.t
  }

val to_string : t -> string

val coords : t -> Vector.t

val click : t -> Vector.t option

