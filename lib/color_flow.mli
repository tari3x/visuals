(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common

type t [@@deriving sexp]

val start_now : Color.t -> t

val add : t -> after:Time.Span.t -> color:Color.t -> t

val eval : t -> Color.t
