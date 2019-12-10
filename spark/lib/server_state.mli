(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

val start : Config.t -> Spark.t list -> ctx:Ctx.t -> unit Lwt.t
