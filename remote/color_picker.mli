(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Util
open Dom_wrappers

val draw : Ctx.t -> unit

val run : _ State_light.t -> Ctx.t -> unit Lwt.t
