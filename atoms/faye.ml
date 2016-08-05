open Js
open Common
open! Printf

(*
let init_called = ref false

let init () =
  if not !init_called
  then begin
    debug "Loading script";
    let script = Html.createScript Html.document in
    let body = get_element_by_id "body" Html.CoerceTo.body in
    Dom.appendChild body script;
    add_event_listener script Html.Event.load ~f:(fun _ ->
      debug "script loaded");
    script##.src := string "http://192.168.0.5:8000/faye/client.js";
    init_called := true
  end
*)

module Channel = struct
  type t = string
  let global = "/global"
  let create () =
    Printf.sprintf "/%d" (Random.int 100_000_000)
end

(* CR: try publishing without converting *)
class type faye = object
  method subscribe : js_string Js.t -> (js_string Js.t -> unit) callback -> unit meth
  method publish : js_string Js.t -> js_string Js.t -> unit meth
end

type 'a t =
  { faye : faye Js.t
  ; to_string : 'a -> string
  }

let constr : (js_string Js.t -> faye Js.t) constr =
  Js.Unsafe.global##._Faye##._Client

let faye_url =
  sprintf "http://%s:8000/faye" (to_string (Html.window##.location##.hostname))

let create ~to_string =
  let faye = new%js constr (string faye_url) in
  { faye; to_string }

let publish (t : 'a t) channel msg =
  (* debug "Publishing %s on %s" (Message.to_string msg) channel; *)
  t.faye##publish (string channel) (Json.output msg)

let subscribe_with_try (t : 'a t) channel ~f =
  let f msg =
    let msg = (Json.unsafe_input msg) in
    (* debug "Received %s on %s" (t.to_string msg) channel; *)
    try f msg
    with
    | Shutdown -> raise Shutdown
    | e -> begin error "%s" (Printexc.to_string e); () end
  in
  (* debug "Subscribed to %s" channel; *)
  t.faye##subscribe
    (string channel)
    (Js.wrap_callback f)
