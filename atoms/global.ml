(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Common

module Channel = Faye.Channel

module Shape_info : sig
  type t
  val create : shape:Shape.t -> owner:Client_id.t option -> t
  val shape : t -> Shape.t
  val owner : t -> Client_id.t option
  val requested : t -> bool
  val last_touched : t -> Time.t
  val set
    :  t
    -> ?shape:Shape.t
    -> ?owner:Client_id.t option
    -> ?requested:bool
    -> unit
    -> t
end = struct
  (* We only modify [shape] and [owner] in [process_message]. The only
     exception is when we add a shape. *)
  type t =
    { shape : Shape.t
    ; owner : Client_id.t option
    (* Set on [request], reset on [release]. *)
    ; requested : bool
    ; last_touched : Time.t
    }

  let create ~shape ~owner =
    let last_touched = Time.now () in
    { shape; owner; requested = false; last_touched }

  let shape t = t.shape
  let owner t = t.owner
  let requested t = t.requested
  let last_touched t = t.last_touched

  let set t
      ?(shape = t.shape)
      ?(owner = t.owner)
      ?(requested = t.requested) () =
    let last_touched = Time.now () in
    { shape; owner; requested; last_touched }

  let _to_string { shape; owner; requested; last_touched } =
    Printf.sprintf "{ shape = %s; owner = %s; requested = %b; last_touched = %s }"
      (Shape.to_string shape)
      (Option.to_string Client_id.to_string owner)
      requested
      (Time.to_string last_touched)
end

module Client_info = struct
  type t =
    { last_active : Time.t
    }
end

type t =
  { faye : Message.t Faye.t
  ; is_server : bool
  ; client_id : Client_id.t
  (* CR: get rid of this, or at least make sure that it is only used to order
     shapes during rendering. *)
  ; mutable shape_ids : Shape_id.t list
  ; shapes : Shape_info.t Hashtbl.M(Shape_id).t
  ; max_clients : int
  ; clients : Client_info.t Hashtbl.M(Client_id).t
  ; mutable server_offset : Time.Span.t
  ; viewport_width  : float
  ; viewport_height : float
  (* Multiply server coordinate by this number (<= 1) to get client
     coordinate. *)
  ; mutable viewport_scale : float
  ; mutable on_change : (Shape_id.t -> Shape.t -> unit) list
  }

let get t shape_id =
  Option.map (Hashtbl.find t.shapes shape_id) ~f:(fun shape ->
    Shape_info.shape shape)

let get_exn t shape_id =
  let shape = Hashtbl.find_exn t.shapes shape_id in
  Shape_info.shape shape

let encode_shape t shape =
  Shape.to_local shape ~viewport_scale:t.viewport_scale

let decode_shape t shape =
  Shape.of_local shape ~viewport_scale:t.viewport_scale

let init_message t =
  let shapes =
    List.map (List.rev t.shape_ids) ~f:(fun shape_id ->
      let shape_info = Hashtbl.find_exn t.shapes shape_id in
      let shape = Shape_info.shape shape_info in
      let owner = Shape_info.owner shape_info in
      (shape_id, encode_shape t shape, owner))
  in
  Message.Init
    { shapes
    ; width = t.viewport_width
    ; height = t.viewport_height
    ; time = Time.now ()
    }

let init t msg =
  let { Message.Init. shapes; width; height; time } = msg in
  let scale_x = t.viewport_width /. width in
  let scale_y = t.viewport_height /. height in
  (* Make sure to set viewport scale before decoding the shapes. *)
  t.viewport_scale <- Float.min (Float.min scale_x scale_y) 1.0;
  List.iter shapes ~f:(fun (shape_id, shape, owner) ->
    let shape = decode_shape t shape in
    t.shape_ids <- shape_id :: t.shape_ids;
    let shape_info = Shape_info.create ~shape ~owner in
    Hashtbl.set t.shapes ~key:shape_id ~data:shape_info);
  t.shape_ids <- List.rev t.shape_ids;
  t.server_offset <- Time.(time - now ())

let is_owner t shape_id =
  match Hashtbl.find t.shapes shape_id with
  | None -> false
  | Some shape_info ->
    match Shape_info.owner shape_info with
    | None -> false
    | Some client_id -> Client_id.equal client_id t.client_id

let assign t client_id shape_id =
  let release () =
    Faye.publish t.faye Channel.global (Message.Release shape_id)
  in
  match Hashtbl.find t.shapes shape_id with
  | None ->
    if Client_id.equal t.client_id client_id then release ()
  | Some shape ->
    if Client_id.equal t.client_id client_id && not (Shape_info.requested shape)
    then release ()
    else begin
      let shape = Shape_info.set shape ~owner:(Some client_id) () in
      Hashtbl.set t.shapes ~key:shape_id ~data:shape;
      (* This adds new shapes to [shape_ids]. *)
      t.shape_ids <- List.bring_to_front t.shape_ids shape_id ~equal:Shape_id.equal;
    end

let call_on_change t shape_id shape =
  List.iter t.on_change ~f:(fun f -> f shape_id shape)

(* Even with correct ordering of messages, if the client's browser suddenly
   dies, it might not release the shape. *)
let cleanup_shapes t =
  let time = Time.now () in
  t.shape_ids <- List.filter t.shape_ids ~f:(fun shape_id ->
    let shape = Hashtbl.find_exn t.shapes shape_id in
    let last_touched = Shape_info.last_touched shape in
    let age = Time.(time - last_touched) |> Time.Span.to_seconds in
    if Float.(age > 30.)
    then begin
      Hashtbl.remove t.shapes shape_id;
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

let change t shape_id ~f =
  match Hashtbl.find t.shapes shape_id with
  | None -> error "shape not found in change"; ()
  | Some shape ->
    Hashtbl.set t.shapes ~key:shape_id ~data:(f shape)

let publish t msg =
  Faye.publish t.faye Channel.global msg

let add t client_id shape_id shape =
  let shape_info = Shape_info.create ~shape ~owner:(Some client_id) in
  Hashtbl.set t.shapes ~key:shape_id ~data:shape_info;
  t.shape_ids <- List.bring_to_front t.shape_ids shape_id ~equal:Shape_id.equal;
  call_on_change t shape_id shape

    (*
let reject_because_max_clients t client_id =
  bla
    *)

let process_message t  = function
  | Message.Request (client_id, shape_id) ->
    if t.is_server
    then change t shape_id ~f:(fun shape ->
      match Shape_info.owner shape with
      | Some _ -> shape
      | None ->
        publish t (Message.Grant (client_id, shape_id));
        Shape_info.set shape ~owner:(Some client_id) ())
  | Message.Release shape_id ->
    change t shape_id ~f:(fun shape ->
      Shape_info.set shape ~owner:None ())
  | Message.Grant (client_id, shape_id) ->
    assign t client_id shape_id
  | Message.Add (shape_id, client_id, shape) ->
    (* Add is called immediately by the owner, to reduce the chance of a race
       condition. *)
    if Client_id.(client_id <> t.client_id)
    then add t client_id shape_id (decode_shape t shape)
  | Message.Set (_, shape_id, shape) ->
    let shape = decode_shape t shape in
    change t shape_id ~f:(fun shape_info ->
      Shape_info.set shape_info ~shape ());
    call_on_change t shape_id shape
  | Message.Delete shape_id ->
    Hashtbl.remove t.shapes shape_id;
    t.shape_ids <- List.delete t.shape_ids shape_id ~equal:Shape_id.equal
  | Message.Request_init (_, channel) ->
    if t.is_server
    then Faye.publish t.faye channel (init_message t)
  | Message.Init msg ->
    if not t.is_server then init t msg
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
  process_message t msg

let create ~viewport_width ~viewport_height ~is_server ~max_clients =
  let faye = Faye.create ~to_string:Message.to_string in
  let t =
    { faye
    ; is_server
    ; client_id = Client_id.create ()
    ; shape_ids = []
    ; shapes = Hashtbl.create (module Shape_id) ()
    ; server_offset = Time.Span.zero
    ; viewport_width
    ; viewport_height
    ; viewport_scale = 1.
    ; on_change = []
    ; max_clients
    ; clients = Hashtbl.create (module Client_id) ()
    }
  in
  Faye.subscribe_with_try t.faye Channel.global ~f:(process_message t);
  if t.is_server
  then begin
    Lwt.every ~span:(Time.Span.of_seconds 1.) ~f:(fun () -> cleanup_shapes t);
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
  t.shape_ids <- List.filter t.shape_ids ~f:(Hashtbl.mem t.shapes);
  (* CR: better reverse in other places. *)
  List.iter (List.rev t.shape_ids) ~f:(fun shape_id ->
    match Hashtbl.find t.shapes shape_id with
    | None ->
      (* CR: this does happen if you don't filter, presumably because of shape
         cleanup. *)
      error "shape_id is not in shapes"
    | Some shape -> f (Shape_info.shape shape))

let find t ~f =
  List.find (List.rev t.shape_ids) ~f:(fun shape_id ->
    match Hashtbl.find t.shapes shape_id with
    | None -> false
    | Some shape -> f (Shape_info.shape shape))

let request t shape_id =
  change t shape_id ~f:(fun shape ->
    publish t (Message.Request (t.client_id, shape_id));
    Shape_info.set shape ~requested:true ())

let release t shape_id =
  change t shape_id ~f:(fun shape ->
    Shape_info.set shape ~requested:true ());
  if is_owner t shape_id
  then publish t (Message.Release shape_id)

let add t shape =
  let shape_id = Shape_id.create () in
  add t t.client_id shape_id shape;
  let shape = encode_shape t shape in
  publish t (Message.Add (shape_id, t.client_id, shape));
  shape_id

let change t shape_id ~f =
  if is_owner t shape_id
  then match Hashtbl.find t.shapes shape_id with
  | None -> ()
  | Some shape ->
    let shape = f (Shape_info.shape shape) in
    let shape = Shape.to_local shape ~viewport_scale:t.viewport_scale in
    publish t (Message.Set (t.client_id, shape_id, shape))

let delete t shape_id =
  if is_owner t shape_id
  then publish t (Message.Delete shape_id)

let now_on_server t =
  Time.(now () + t.server_offset)

let on_change t ~f =
  t.on_change <- f :: t.on_change
