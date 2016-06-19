open Js
open Common

module Channel = struct
  type t = string
  let global = "/global"
  let create () =
    Printf.sprintf "/%d" (Random.int 100_000_000)
end

module Message = struct
  type t =
  | Request of (Client_id.t * Shape_id.t)
  | Grant   of (Client_id.t * Shape_id.t)
  | Release of Shape_id.t
  | Create  of (Shape_id.t * Client_id.t * Shape.t)
  | Set     of (Shape_id.t * Shape.t)
  | Delete  of Shape_id.t
  | Request_state of Channel.t
  | State  of (Shape_id.t * Shape.t * Client_id.t option) list

  let to_string = function
    | Request _ -> "Request"
    | Grant _   -> "Grant"
    | Release _ -> "Release"
    | Create _  -> "Create"
    | Set (_, shape)     -> Printf.sprintf "Set %s" (Shape.to_string shape)
    | Delete _  -> "Delete"
    | State _   -> "State"
    | Request_state _ -> "Request_state"

end

(* CR: try publishing without converting *)
class type faye = object
  method subscribe : js_string Js.t -> (js_string Js.t -> unit) callback -> unit meth
  method publish : js_string Js.t -> js_string Js.t -> unit meth
end

type t = faye Js.t

let constr : (js_string Js.t -> t) constr =
  Js.Unsafe.global##._Faye##._Client

let create ~url =
  new%js constr (string url)

let publish t channel msg =
  (* debug "Publishing %s on %s" (Message.to_string msg) channel; *)
  t##publish (string channel) (Json.output msg)

let subscribe t channel ~f =
  let f msg =
    let msg = (Json.unsafe_input msg) in
    (* debug "Received %s on %s" (Message.to_string msg) channel; *)
    f msg
  in
  (* debug "Subscribed to %s" channel; *)
  t##subscribe
    (string channel)
    (Js.wrap_callback f)
