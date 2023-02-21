(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

(* CR: convert this to create arguments. *)
module Config : sig
  type t =
    { max_box_age : Time.Span.t
    ; global_channel_name : string
    }
end

type 'a t

val create_exn
  :  Config.t
  -> Ctx.t
  -> 'a Box.t
  -> sexp_of_a:('a -> Sexp.t)
  -> 'a t Lwt.t

val process_action : _ t -> Action.t -> unit
val set_color : _ t -> Color.t -> unit
