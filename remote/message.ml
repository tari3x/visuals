(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Util
open Common

module Init = struct
  type 'a t =
    { boxes : (Box_id.t * 'a Box.Local.t * Client_id.t option) list
    ; width : float
    ; height : float
    ; time : Time.t
    } [@@deriving sexp]
end

(* CR: move client_id out *)
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
let to_string = function
  | Request _ -> "Request"
  | Grant _   -> "Grant"
  | Release _ -> "Release"
  | Add _  -> "Add"
  | Set (_, _, shape)  -> Printf.sprintf "Set %s" (Box.Local.to_string shape)
  | Delete _  -> "Delete"
  | Init _   -> "Init"
  | Request_init _ -> "Request_init"
  | Max_clients_exceeded (_, max_clients) ->
    Printf.sprintf "Max_clients_exceeded %d" max_clients
    *)

let client_id = function
  | Request (client_id, _) -> Some client_id
  | Release _ -> None
  | Grant (client_id, _) -> Some client_id
  | Add (client_id, _, _) -> Some client_id
  | Set (client_id, _, _) -> Some client_id
  | Delete _ -> None
  | Request_init (client_id, _) -> Some client_id
  | Init _ -> None
  | Max_clients_exceeded _ -> None
