(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

module Pointer = Action.Pointer

type t

module Update : sig
  module Single : sig
    type t
    val apply : t -> 'a Box.t -> 'a Box.t
  end

  (* [None] means the shape is not touched any more. *)
  type t = (Box_id.t * Single.t option) list
end

val create : unit -> t

val is_touching : t -> Box_id.t -> bool

val add : t -> Box_id.t -> Frame.t -> Pointer.t -> Update.t

val move : t -> Pointer.t list -> Update.t

val remove : t -> Pointer.t list -> Update.t

val active : t -> Box_id.t list
