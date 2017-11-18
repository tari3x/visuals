(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Geometry

val default_line_width : float

type 'a t = private
  { kind : 'a
  ; frame : Frame.t
  ; color : Color_cycle.t
  ; line_width : float
  (* in the internal coordinate system of the box *)
  ; touches : Vector.t list
  } [@@deriving fields, sexp]

val default : 'a -> 'a t

(* val to_string : _ t -> string  *)

val create
  :  kind:'a
  -> ?frame:Frame.t
  -> ?color:Color_cycle.t
  -> ?line_width:float
  -> unit
  -> 'a t

val set
  :  ?kind:'a
  -> ?frame:Frame.t
  -> ?color:Color_cycle.t
  -> ?line_width:float
  -> 'a t
  -> 'a t

val set_touches
  : 'a t
  -> coordinates:[ `internal | `canvas ]
  -> touches:Vector.t list
  -> 'a t

val set_translation : 'a t -> Vector.t -> 'a t
val set_alpha : 'a t -> alpha:float -> 'a t

(* Box together with the viewport scale in which it was created. *)
module Local : sig
  type 'a t [@@deriving sexp]
  (* val to_string : t -> string *)
end

val to_local : 'a t -> viewport_scale:float -> 'a Local.t
val of_local : 'a Local.t -> viewport_scale:float -> 'a t
