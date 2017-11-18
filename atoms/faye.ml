(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
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
  include String

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
  ; sexp_of_a : ('a -> Sexp.t)
  ; buffers : 'a Ordered_stream.t Hashtbl.M(Channel).t
  }

let constr : (js_string Js.t -> faye Js.t) constr =
  Js.Unsafe.global##._Faye##._Client

let faye_url =
  sprintf "http://%s:8000/faye" (to_string (Html.window##.location##.hostname))

let create ~sexp_of_a =
  let faye = new%js constr (string faye_url) in
  let buffers = Hashtbl.create (module Channel) () in
  { faye; sexp_of_a; buffers }

let get_buffer t channel =
  Hashtbl.find_or_add t.buffers channel ~default:(fun () ->
    Ordered_stream.create ~max_buffer_size:5)

let publish (t : 'a t) channel msg =
  let buffer = get_buffer t channel in
  (* debug "Publishing %s on %s" (Message.to_string msg) channel; *)
  let msg = Ordered_stream.create_element buffer msg in
  t.faye##publish (string channel) (Json.output msg)

let subscribe_with_try (t : 'a t) channel ~f =
  let buffer = get_buffer t channel in
  let write_to_buffer msg =
    let msg = Json.unsafe_input msg in
    Ordered_stream.write buffer msg
  in
  let read_from_buffer msg =
    (* debug !"Received %{Message} on %{Channel" msg_string channel; *)
    f msg
  in
  (* debug "Subscribed to %s" channel; *)
  t.faye##subscribe
    (string channel)
    (Js.wrap_callback write_to_buffer);
  Lwt.async (fun () ->
    Lwt_stream.iter_with_try (Ordered_stream.reader buffer) ~f:read_from_buffer)
