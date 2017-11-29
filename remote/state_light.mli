(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Util
open Dom_wrappers

type 'a t

val create
  :   Ctx.t
  -> 'a Box.t
  -> sexp_of_a:('a -> Sexp.t)
  -> 'a t Lwt.t

val process_action : _ t -> Action.t -> unit

val set_color : _ t -> Color.t -> unit
