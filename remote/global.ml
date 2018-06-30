(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Std_internal

module Channel = Faye.Channel

module Config = struct
  type t =
    { viewport_width : float
    ; viewport_height : float
    ; is_server : bool
    (* CR-someday: fold this into [is_server] *)
    ; max_clients : int
    ; max_box_age : Time.Span.t
    ; global_channel_name : string
    }
end

module Box_info : sig
  type 'a t
  val create : box:'a Box.t -> owner:Client_id.t option -> 'a t
  val box : 'a t -> 'a Box.t
  val owner : _ t -> Client_id.t option
  val requested : _ t -> bool
  val last_touched : _ t -> Time.t
  val set
    :  'a t
    -> ?box:'a Box.t
    -> ?owner:Client_id.t option
    -> ?requested:bool
    -> unit
    -> 'a t
end = struct
  (* We only modify [box] and [owner] in [process_message]. The only
     exception is when we add a box. *)
  type 'a t =
    { box : 'a Box.t
    ; owner : Client_id.t option
    (* Set on [request], reset on [release]. *)
    ; requested : bool
    ; last_touched : Time.t
    }

  let create ~box ~owner =
    let last_touched = Time.now () in
    { box; owner; requested = false; last_touched }

  let box t = t.box
  let owner t = t.owner
  let requested t = t.requested
  let last_touched t = t.last_touched

  let set t
      ?(box = t.box)
      ?(owner = t.owner)
      ?(requested = t.requested) () =
    let last_touched = Time.now () in
    { box; owner; requested; last_touched }
end

module Client_info = struct
  type t =
    { last_active : Time.t
    }
end

type 'a t =
  { config : Config.t
  ; faye : 'a Message.t Faye.t
  ; client_id : Client_id.t
  (* CR: get rid of this, or at least make sure that it is only used to order
     boxs during rendering. *)
  ; mutable box_ids : Box_id.t list
  ; boxes : 'a Box_info.t Hashtbl.M(Box_id).t
  ; clients : Client_info.t Hashtbl.M(Client_id).t
  ; mutable server_offset : Time.Span.t
  (* Multiply server coordinate by this number (<= 1) to get client
     coordinate. *)
  ; mutable viewport_scale : float
  ; mutable on_change : (Box_id.t -> 'a Box.t -> unit) list
  ; sexp_of_a : ('a -> Sexp.t)
  }

let get t box_id =
  Option.map (Hashtbl.find t.boxes box_id) ~f:(fun box ->
    Box_info.box box)

let get_exn t box_id =
  let box = Hashtbl.find_exn t.boxes box_id in
  Box_info.box box

let encode_box t box =
  Box.to_local box ~viewport_scale:t.viewport_scale

let decode_box t box =
  Box.of_local box ~viewport_scale:t.viewport_scale

let init_message t =
  let boxes =
    List.map (List.rev t.box_ids) ~f:(fun box_id ->
      let box_info = Hashtbl.find_exn t.boxes box_id in
      let box = Box_info.box box_info in
      let owner = Box_info.owner box_info in
      (box_id, encode_box t box, owner))
  in
  Message.Init
    { boxes
    ; width = t.config.viewport_width
    ; height = t.config.viewport_height
    ; time = Time.now ()
    }

let init t msg =
  let { Message.Init. boxes; width; height; time } = msg in
  let scale_x = t.config.viewport_width /. width in
  let scale_y = t.config.viewport_height /. height in
  (* Make sure to set viewport scale before decoding the boxes. *)
  t.viewport_scale <- Float.min (Float.min scale_x scale_y) 1.0;
  List.iter boxes ~f:(fun (box_id, box, owner) ->
    let box = decode_box t box in
    t.box_ids <- box_id :: t.box_ids;
    let box_info = Box_info.create ~box ~owner in
    Hashtbl.set t.boxes ~key:box_id ~data:box_info);
  t.box_ids <- List.rev t.box_ids;
  t.server_offset <- Time.(time - now ())

let is_owner t box_id =
  match Hashtbl.find t.boxes box_id with
  | None -> false
  | Some box_info ->
    match Box_info.owner box_info with
    | None -> false
    | Some client_id -> Client_id.equal client_id t.client_id

let assign t client_id box_id =
  let release () =
    Faye.publish t.faye
      (Channel.global t.config.global_channel_name)
      (Message.Release box_id)
  in
  match Hashtbl.find t.boxes box_id with
  | None ->
    if Client_id.equal t.client_id client_id then release ()
  | Some box ->
    if Client_id.equal t.client_id client_id && not (Box_info.requested box)
    then release ()
    else begin
      let box = Box_info.set box ~owner:(Some client_id) () in
      Hashtbl.set t.boxes ~key:box_id ~data:box;
      (* This adds new boxes to [box_ids]. *)
      t.box_ids <- List.bring_to_front t.box_ids box_id ~equal:Box_id.equal;
    end

let call_on_change t box_id box =
  List.iter t.on_change ~f:(fun f -> f box_id box)

(* Even with correct ordering of messages, if the client's browser suddenly
   dies, it might not release the box. *)
let cleanup_boxes t =
  let time = Time.now () in
  t.box_ids <- List.filter t.box_ids ~f:(fun box_id ->
    let box = Hashtbl.find_exn t.boxes box_id in
    let last_touched = Box_info.last_touched box in
    let age = Time.(time - last_touched) in
    if Time.Span.(age > t.config.max_box_age)
    then begin
      Hashtbl.remove t.boxes box_id;
      false
    end
    else true)

    (*
let cleanup_clients t =
  Hashtbl.filter_map_inplace t.clients ~f:(fun ~key:_ ~data:client_info ->
    let last_active = Time.to_seconds client_info.last_active in
    let now = Time.to_seconds (Time.now ()) in
    if now -. last_active > 20. then None else (Some client_info))
    *)

let change t box_id ~f =
  match Hashtbl.find t.boxes box_id with
  | None -> error "box not found in change"; ()
  | Some box ->
    Hashtbl.set t.boxes ~key:box_id ~data:(f box)

let publish t msg =
  Faye.publish t.faye (Channel.global t.config.global_channel_name) msg

let add t client_id box_id box =
  let box_info = Box_info.create ~box ~owner:(Some client_id) in
  Hashtbl.set t.boxes ~key:box_id ~data:box_info;
  t.box_ids <- List.bring_to_front t.box_ids box_id ~equal:Box_id.equal;
  call_on_change t box_id box

    (*
let reject_because_max_clients t client_id =
  bla
    *)

let process_message t = function
  | Message.Request (client_id, box_id) ->
    if t.config.is_server
    then change t box_id ~f:(fun box ->
      match Box_info.owner box with
      | Some _ -> box
      | None ->
        publish t (Message.Grant (client_id, box_id));
        Box_info.set box ~owner:(Some client_id) ())
  | Message.Release box_id ->
    change t box_id ~f:(fun box ->
      Box_info.set box ~owner:None ())
  | Message.Grant (client_id, box_id) ->
    assign t client_id box_id
  | Message.Add (client_id, box_id, box) ->
    (* Add is called immediately by the owner, to reduce the chance of a race
       condition. *)
    if Client_id.(client_id <> t.client_id)
    then add t client_id box_id (decode_box t box)
  | Message.Set (_, box_id, box) ->
    let box = decode_box t box in
    change t box_id ~f:(fun box_info ->
      Box_info.set box_info ~box ());
    call_on_change t box_id box
  | Message.Delete box_id ->
    Hashtbl.remove t.boxes box_id;
    t.box_ids <- List.delete t.box_ids box_id ~equal:Box_id.equal
  | Message.Request_init (_, channel) ->
    if t.config.is_server
    then Faye.publish t.faye channel (init_message t)
  | Message.Init msg ->
    if not t.config.is_server then init t msg
  | Message.Max_clients_exceeded (_client_id, max_clients) ->
    alert "At most %d users are allowed." max_clients;
    raise Shutdown

let process_message t msg =
  (*
  if t.is_server
  then begin
    cleanup_clients t;
    Option.iter (Message.client_id msg) ~f:(fun client_id ->
      if Hashtbl.length t.clients >= t.max_clients
      then Faye.publish t.faye Channel.global
        (Message.Max_clients_exceeded (client_id, t.max_clients))
      else begin
        let client_info = { Client_info. last_active = Time.now () } in
        Hashtbl.replace t.clients ~key:client_id ~data:client_info
      end);
  end;
  *)
  (*
  debug !"processing messages %{Sexp}"
    (Message.sexp_of_t t.sexp_of_a msg);
  *)
  process_message t msg

let create config ~sexp_of_a =
  let faye =
    Faye.create ~sexp_of_a:(Message.sexp_of_t sexp_of_a)
  in
  let t =
    { config
    ; faye
    ; client_id = Client_id.create ()
    ; box_ids = []
    ; boxes = Hashtbl.create (module Box_id) ()
    ; server_offset = Time.Span.zero
    ; viewport_scale = 1.
    ; on_change = []
    ; clients = Hashtbl.create (module Client_id) ()
    ; sexp_of_a
    }
  in
  (* debug !"starting with client_id %{Client_id}" t.client_id; *)
  Faye.subscribe_with_try t.faye
    (Channel.global config.global_channel_name)
    ~f:(process_message t);
  if t.config.is_server
  then begin
    Lwt.every (Time.Span.of_sec 1.) ~f:(fun () -> cleanup_boxes t);
    return t
  end
  else begin
    let channel = Channel.create () in
    Lwt.wrap (fun cont ->
      Faye.subscribe_with_try t.faye channel ~f:(fun msg ->
        process_message t msg;
        cont ());
      publish t (Message.Request_init (t.client_id, channel)))
    >>= fun () ->
    return t
  end

let iter t ~f =
  t.box_ids <- List.filter t.box_ids ~f:(Hashtbl.mem t.boxes);
  (* CR: better reverse in other places. *)
  List.iter (List.rev t.box_ids) ~f:(fun box_id ->
    match Hashtbl.find t.boxes box_id with
    | None ->
      (* CR: this does happen if you don't filter, presumably because of box
         cleanup. *)
      error "box_id is not in boxes"
    | Some box -> f (Box_info.box box))

let find t ~f =
  List.find (List.rev t.box_ids) ~f:(fun box_id ->
    match Hashtbl.find t.boxes box_id with
    | None -> false
    | Some box -> f (Box_info.box box))

let request t box_id =
  change t box_id ~f:(fun box ->
    publish t (Message.Request (t.client_id, box_id));
    Box_info.set box ~requested:true ())

let release t box_id =
  change t box_id ~f:(fun box ->
    Box_info.set box ~requested:false ());
  if is_owner t box_id
  then publish t (Message.Release box_id)

let add t box =
  let box_id = Box_id.create () in
  add t t.client_id box_id box;
  let box = encode_box t box in
  publish t (Message.Add (t.client_id, box_id, box));
  box_id

let change t box_id ~f =
  if is_owner t box_id
  then match Hashtbl.find t.boxes box_id with
  | None -> ()
  | Some box ->
    let box = f (Box_info.box box) in
    let box = Box.to_local box ~viewport_scale:t.viewport_scale in
    publish t (Message.Set (t.client_id, box_id, box))

let delete t box_id =
  if is_owner t box_id
  then publish t (Message.Delete box_id)

let now_on_server t =
  Time.(now () + t.server_offset)

let on_change t ~f =
  t.on_change <- f :: t.on_change
