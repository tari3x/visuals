(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

module Config : sig
  type t =
    (* included in drawing mode *)
    { include_black_strip : bool
    } [@@deriving sexp]
end

type t

val create : Config.t -> Ctx.t -> t

val draw : t -> unit

val run : t -> _ State_light.t  -> unit Lwt.t
