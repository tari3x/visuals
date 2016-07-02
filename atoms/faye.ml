open Js
open Common

module Channel = struct
  type t = string
  let global = "/global"
  let create () =
    Printf.sprintf "/%d" (Random.int 100_000_000)
end

module Message = struct
  module Init = struct
    type t =
      { shapes : (Shape_id.t * Shape.t * Client_id.t option) list
      ; width : float
      ; height : float
      ; time : float
      }
  end

  type t =
  | Request of (Client_id.t * Shape_id.t)
  | Grant   of (Client_id.t * Shape_id.t)
  | Release of Shape_id.t
  | Create  of (Shape_id.t * Client_id.t * Shape.t)
  | Set     of (Shape_id.t * Shape.t)
  | Delete  of Shape_id.t
  | Request_init of Channel.t
  | Init    of Init.t

  let to_string = function
    | Request _ -> "Request"
    | Grant _   -> "Grant"
    | Release _ -> "Release"
    | Create _  -> "Create"
    | Set (_, shape)     -> Printf.sprintf "Set %s" (Shape.to_string shape)
    | Delete _  -> "Delete"
    | Init _   -> "Init"
    | Request_init _ -> "Request_init"

end

(* CR: try publishing without converting *)
class type faye = object
  method subscribe : js_string Js.t -> (js_string Js.t -> unit) callback -> unit meth
  method publish : js_string Js.t -> js_string Js.t -> unit meth
end

type t = faye Js.t

let constr : (js_string Js.t -> t) constr =
  Js.Unsafe.global##._Faye##._Client

let create () =
  new%js constr (string "http://192.168.1.100:8000/faye")

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
