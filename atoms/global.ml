open Lwt
open Common

module Channel = Faye.Channel

module Shape_info = struct
  (* We only modify [shape] and [owner] in [process_message]. The only
     exception is when we add a shape. *)
  type t =
    { shape : Shape.t
    ; owner : Client_id.t option
    (* Set on [request], reset on [release]. *)
    ; requested : bool
    }

  let create ~shape ~owner =
    { shape; owner; requested = false }

  let _to_string { shape; owner; requested } =
    Printf.sprintf "{ shape = %s; owner = %s; requested = %b"
      (Shape.to_string shape)
      (Option.to_string Client_id.to_string owner)
      requested
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
  ; mutable shape_ids : Shape_id.t list
  ; shapes : (Shape_id.t, Shape_info.t) Hashtbl.t
  ; max_clients : int
  ; clients : (Client_id.t, Client_info.t) Hashtbl.t
  ; mutable server_offset : Time.Span.t
  ; viewport_width  : float
  ; viewport_height : float
  (* Multiply server coordinate by this number (<= 1) to get client
     coordinate. *)
  ; mutable viewport_scale : float
  ; mutable on_change : (Shape_id.t -> Shape.t -> unit) list
  }

let get t shape_id =
  Option.map (Hashtbl.maybe_find t.shapes shape_id) ~f:(fun shape ->
    shape.shape)

let get_exn t shape_id =
  let shape = Hashtbl.find t.shapes shape_id in
  shape.shape

let encode_shape t shape =
  Shape.to_local shape ~viewport_scale:t.viewport_scale

let decode_shape t shape =
  Shape.of_local shape ~viewport_scale:t.viewport_scale

let init_message t =
  let shapes =
    List.map (List.rev t.shape_ids) ~f:(fun shape_id ->
      let { Shape_info. shape; owner; requested = _ } =
        Hashtbl.find t.shapes shape_id
      in
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
  t.viewport_scale <- min (min scale_x scale_y) 1.0;
  List.iter shapes ~f:(fun (shape_id, shape, owner) ->
    let shape = decode_shape t shape in
    t.shape_ids <- shape_id :: t.shape_ids;
    let shape_info = Shape_info.create ~shape ~owner in
    Hashtbl.add t.shapes shape_id shape_info);
  t.shape_ids <- List.rev t.shape_ids;
  t.server_offset <- Time.(time - now ())

let is_owner t shape_id =
  match Hashtbl.maybe_find t.shapes shape_id with
  | Some { owner = Some client_id; _ } when client_id = t.client_id -> true
  | _ -> false

let assign t client_id shape_id =
  let release () =
    Faye.publish t.faye Channel.global (Message.Release shape_id)
  in
  match Hashtbl.maybe_find t.shapes shape_id with
  | None ->
    if t.client_id = client_id then release ()
  | Some shape ->
    if t.client_id = client_id && not shape.requested
    then release ()
    else begin
      let shape = { shape with owner = Some client_id } in
      Hashtbl.replace t.shapes ~key:shape_id ~data:shape;
      (* This adds new shapes to [shape_ids]. *)
      t.shape_ids <- List.bring_to_front t.shape_ids shape_id;
    end

let call_on_change t shape_id shape =
  List.iter t.on_change ~f:(fun f -> f shape_id shape)

    (*
let cleanup_clients t =
  Hashtbl.filter_map_inplace t.clients ~f:(fun ~key:_ ~data:client_info ->
    let last_active = Time.to_seconds client_info.last_active in
    let now = Time.to_seconds (Time.now ()) in
    if now -. last_active > 20. then None else (Some client_info))
    *)

let change t shape_id ~f =
  match Hashtbl.maybe_find t.shapes shape_id with
  | None -> error "shape not found in change"; ()
  | Some shape ->
    Hashtbl.replace t.shapes ~key:shape_id ~data:(f shape)

let publish t msg =
  Faye.publish t.faye Channel.global msg

let add t client_id shape_id shape =
  let shape_info = Shape_info.create ~shape ~owner:(Some client_id) in
  Hashtbl.replace t.shapes ~key:shape_id ~data:shape_info;
  t.shape_ids <- List.bring_to_front t.shape_ids shape_id;
  call_on_change t shape_id shape

    (*
let reject_because_max_clients t client_id =
  bla
    *)

let process_message t  = function
  | Message.Request (client_id, shape_id) ->
    if t.is_server
    then change t shape_id ~f:(fun shape ->
      match shape.owner with
      | Some _ -> shape
      | None ->
        publish t (Message.Grant (client_id, shape_id));
        { shape with owner = Some client_id })
  | Message.Release shape_id ->
    change t shape_id ~f:(fun shape ->
      { shape with owner = None } )
  | Message.Grant (client_id, shape_id) ->
    assign t client_id shape_id
  | Message.Add (shape_id, client_id, shape) ->
    (* Add is called immediately by the owner, to reduce the chance of a race
       condition. *)
    if client_id <> t.client_id
    then add t client_id shape_id (decode_shape t shape)
  | Message.Set (_, shape_id, shape) ->
    let shape = decode_shape t shape in
    change t shape_id ~f:(fun shape_info -> { shape_info with shape } );
    call_on_change t shape_id shape
  | Message.Delete shape_id ->
    Hashtbl.remove t.shapes shape_id;
    t.shape_ids <- List.delete t.shape_ids shape_id
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
    ; shapes = Hashtbl.create ()
    ; server_offset = Time.Span.zero
    ; viewport_width
    ; viewport_height
    ; viewport_scale = 1.
    ; on_change = []
    ; max_clients
    ; clients = Hashtbl.create ()
    }
  in
  Faye.subscribe_with_try t.faye Channel.global ~f:(process_message t);
  let channel = Channel.create () in
  if t.is_server then return t
  else begin
    publish t (Message.Request_init (t.client_id, channel));
    lwt_wrap (fun cont ->
      Faye.subscribe_with_try t.faye channel ~f:(fun msg ->
        process_message t msg;
        cont ()))
    >>= fun () ->
    return t
  end

let iter t ~f =
  (* CR: better reverse in other places. *)
  List.iter (List.rev t.shape_ids) ~f:(fun shape_id ->
    match Hashtbl.maybe_find t.shapes shape_id with
    | None ->
      (* CR: does this happen, and why? *)
      error "shape_id is not in shapes"
    | Some shape -> f shape.shape)

let find t ~f =
  List.maybe_find (List.rev t.shape_ids) ~f:(fun shape_id ->
    match Hashtbl.maybe_find t.shapes shape_id with
    | None -> false
    | Some shape -> f shape.shape)

let request t shape_id =
  change t shape_id ~f:(fun shape ->
    publish t (Message.Request (t.client_id, shape_id));
    { shape with requested = true })

let release t shape_id =
  change t shape_id ~f:(fun shape -> { shape with requested = false });
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
  then match Hashtbl.maybe_find t.shapes shape_id with
  | None -> ()
  | Some shape ->
    let shape = f shape.shape in
    let shape = Shape.to_local shape ~viewport_scale:t.viewport_scale in
    publish t (Message.Set (t.client_id, shape_id, shape))

let delete t shape_id =
  if is_owner t shape_id
  then publish t (Message.Delete shape_id)

let now_on_server t =
  Time.(now () + t.server_offset)

let on_change t ~f =
  t.on_change <- f :: t.on_change
