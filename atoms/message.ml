open Common

module Init = struct
  type t =
    { shapes : (Shape_id.t * Shape.Local.t * Client_id.t option) list
    ; width : float
    ; height : float
    ; time : Time.t
    }
end

(* CR: move client_id out *)
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

let to_string = function
  | Request _ -> "Request"
  | Grant _   -> "Grant"
  | Release _ -> "Release"
  | Add _  -> "Add"
  | Set (_, _, shape)  -> Printf.sprintf "Set %s" (Shape.Local.to_string shape)
  | Delete _  -> "Delete"
  | Init _   -> "Init"
  | Request_init _ -> "Request_init"
  | Max_clients_exceeded (_, max_clients) ->
    Printf.sprintf "Max_clients_exceeded %d" max_clients

let client_id = function
  | Request (client_id, _) -> Some client_id
  | Release _ -> None
  | Grant (client_id, _) -> Some client_id
  | Add (_, client_id, _) -> Some client_id
  | Set (client_id, _, _) -> Some client_id
  | Delete _ -> None
  | Request_init (client_id, _) -> Some client_id
  | Init _ -> None
  | Max_clients_exceeded _ -> None
