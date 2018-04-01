(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common

type t

val create : half_life:Time.Span.t -> time:Time.t -> value:float -> t

val add_sample : t -> time:Time.t -> value:float -> unit

val value : t -> float
