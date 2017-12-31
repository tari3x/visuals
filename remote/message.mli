(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

module Init : sig
  type 'a t =
    { boxes : (Box_id.t * 'a Box.Local.t * Client_id.t option) list
    ; width : float
    ; height : float
    ; time : Time.t
    } [@@deriving sexp]
end

type 'a t =
| Request of (Client_id.t * Box_id.t)
| Grant   of (Client_id.t * Box_id.t)
| Release of Box_id.t
| Add     of (Client_id.t * Box_id.t * 'a Box.Local.t)
| Set     of (Client_id.t * Box_id.t * 'a Box.Local.t)
| Delete  of Box_id.t
| Request_init of (Client_id.t * Faye.Channel.t)
| Init    of 'a Init.t
| Max_clients_exceeded of (Client_id.t * int)
    [@@deriving sexp]

(*
val to_string : _ t -> string
*)

val client_id : _ t -> Client_id.t option
