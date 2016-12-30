(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common

module Init : sig
  type t =
    { shapes : (Shape_id.t * Shape.Local.t * Client_id.t option) list
    ; width : float
    ; height : float
    ; time : Time.t
    }
end

type t =
| Request of (Client_id.t * Shape_id.t)
| Grant   of (Client_id.t * Shape_id.t)
| Release of Shape_id.t
| Add     of (Shape_id.t * Client_id.t * Shape.Local.t)
| Set     of (Client_id.t * Shape_id.t * Shape.Local.t)
| Delete  of Shape_id.t
| Request_init of (Client_id.t * Faye.Channel.t)
| Init    of Init.t
| Max_clients_exceeded of (Client_id.t * int)

val to_string : t -> string

val client_id : t -> Client_id.t option
