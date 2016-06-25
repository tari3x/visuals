open Lwt
open Common
open Dom_wrappers

module Channel = Faye.Channel
module Message = Faye.Message

(* CR: try D3 *)

(* CR: bug if phone disconnects. *)

type t =
  { ctx : Ctx.t
  ; faye : Faye.t
  ; is_leader : bool
  ; client_id : Client_id.t
  (* CR: only redraw shapes that have owners *)
  ; mutable shape_ids : Shape_id.t list
  ; shapes : (Shape_id.t, Shape.t) Hashtbl.t
  ; owners : (Shape_id.t, Client_id.t) Hashtbl.t
  ; mutable last_touched : Shape_id.t
  ; touches : Multitouch.t
  ; mutable active_color : Color_cycle.t
  ; mutable on_shape_active : (Shape.t -> unit) list
  (* This is synchronized with the master for two purposes. First, so that
     loops look somewhat synchronous on all devices. Second, to make it more
     likely that times are positive in all computations. *)
  ; mutable time_offset : float
  }

let last_touched t =
  Hashtbl.maybe_find t.shapes t.last_touched

let last_touched_still_touching t =
  if Multitouch.is_touching t.touches t.last_touched
  then Some (t.last_touched, Hashtbl.find t.shapes t.last_touched)
  else None

let rec render_loop t =
  Lwt_js.sleep 0.1
  >>= fun () ->
  let time = Unix.gettimeofday () in
  Ctx.clear t.ctx;
  (* CR: better reverse in other places. *)
  List.iter (List.rev t.shape_ids) ~f:(fun shape_id ->
    let shape = Hashtbl.find t.shapes shape_id in
    Shape.render shape t.ctx ~time);
  render_loop t

let is_owner t shape_id =
  match Hashtbl.maybe_find t.owners shape_id with
  | None -> false
  | Some client_id -> client_id = t.client_id

let assign t client_id shape_id =
  Hashtbl.replace t.owners ~key:shape_id ~data:client_id;
  (* This also adds new shapes to [shape_ids]. *)
  t.shape_ids <- List.bring_to_front t.shape_ids shape_id;
  let shape = Hashtbl.find t.shapes shape_id in
  if t.client_id = client_id && not (Multitouch.is_touching t.touches shape_id)
  then Faye.publish t.faye Channel.global (Message.Release shape_id)
  else List.iter t.on_shape_active ~f:(fun f -> f shape)

let release t shape_id =
  Hashtbl.remove t.owners shape_id

let process_message t = function
  | Message.Request (client_id, shape_id) ->
    if t.is_leader && not (Hashtbl.mem t.owners shape_id)
    then begin
      Hashtbl.replace t.owners ~key:shape_id ~data:client_id;
      Faye.publish t.faye Channel.global
        (Message.Grant (client_id, shape_id))
    end
  | Message.Release shape_id ->
    release t shape_id
  | Message.Grant (client_id, shape_id) ->
    assign t client_id shape_id
  | Message.Create (shape_id, client_id, shape) ->
    Hashtbl.replace t.shapes ~key:shape_id ~data:shape;
    assign t client_id shape_id
  | Message.Set (shape_id, shape) ->
    Hashtbl.replace t.shapes ~key:shape_id ~data:shape
  | Message.Delete shape_id ->
    Hashtbl.remove t.shapes shape_id;
    t.shape_ids <- List.delete t.shape_ids shape_id;
    release t shape_id
  | Message.Request_state channel ->
    if t.is_leader
    then begin
      let state =
        List.map (List.rev t.shape_ids) ~f:(fun shape_id ->
          let shape = Hashtbl.find t.shapes shape_id in
          let client_id = Hashtbl.maybe_find t.owners shape_id in
          (shape_id, shape, client_id))
      in
      Faye.publish t.faye channel (Message.State state)
    end
  | Message.State state ->
    List.iter state ~f:(fun (shape_id, shape, client_id) ->
      t.shape_ids <- shape_id :: t.shape_ids;
      Hashtbl.add t.shapes shape_id shape;
      Option.iter client_id ~f:(fun client_id ->
        Hashtbl.add t.owners shape_id client_id));
    t.shape_ids <- List.rev t.shape_ids

(* CR-someday: I'm not sure I have to actually wait for the window to load,
   but I suspect I do. *)
let create ctx ~is_leader =
  let faye = Faye.create ~url:"http://192.168.1.100:8000/faye" in
  let t =
    { ctx
    ; faye
    ; is_leader
    ; client_id = Client_id.create ()
    ; shape_ids = []
    ; shapes = Hashtbl.create ()
    ; owners = Hashtbl.create ()
    (* Start with a bit of lies, never lie after that. *)
    ; last_touched = Shape_id.create ()
    ; touches = Multitouch.create ()
    ; active_color = Color_cycle.default
    ; on_shape_active = []
    ; time_offset = 0.
    }
  in
  Faye.subscribe t.faye Channel.global ~f:(process_message t);
  let channel = Channel.create () in
  Faye.publish t.faye Channel.global (Message.Request_state channel);
  Faye.subscribe t.faye channel ~f:(process_message t);
  Lwt.async (fun () -> render_loop t);
  t

let set_shape t shape_id shape =
  if not (is_owner t shape_id) then []
  else [ Message.Set (shape_id, shape) ]

let process_pointer_action t (action : Action.Pointer_action.t) =
  match action.kind with
  | `move ->
    begin
      Multitouch.move t.touches action.changed_touches
      |> List.map ~f:(fun (shape_id, frame) ->
        let shape = Hashtbl.find t.shapes shape_id in
        let shape = Shape.set_frame shape frame in
        set_shape t shape_id shape)
      |> List.concat
    end
  | `down ->
    List.concat_map action.changed_touches ~f:(fun touch ->
      let shape_id =
        List.maybe_find t.shape_ids ~f:(fun shape_id ->
          let shape = Hashtbl.find t.shapes shape_id in
          Shape.touched_by shape touch.position)
      in
      match shape_id with
      | Some shape_id ->
        let shape = Hashtbl.find t.shapes shape_id in
        Multitouch.add t.touches shape_id (Shape.frame shape) touch;
        t.last_touched <- shape_id;
        [ Message.Request (t.client_id, shape_id) ]
      | None ->
        match last_touched_still_touching t with
        | Some (shape_id, shape) ->
          Multitouch.add t.touches shape_id (Shape.frame shape) touch;
          []
        | None ->
          let shape_id = Shape_id.create () in
          let shape = Shape.create touch.position t.active_color in
          Multitouch.add t.touches shape_id (Shape.frame shape) touch;
          t.last_touched <- shape_id;
          [ Message.Create (shape_id, t.client_id, shape) ]
    )
  | `up ->
    Multitouch.remove t.touches action.changed_touches
    |> List.map ~f:(fun shape_id ->
      Message.Release shape_id)

let process_action t = function
  | Action.Pointer action -> process_pointer_action t action
  | Action.Set_color color ->
    t.active_color <- color;
    match last_touched_still_touching t with
    | None -> []
    | Some (shape_id, shape) ->
      debug "Setting color";
      let shape = Shape.set_color shape color in
      set_shape t shape_id shape

let process_action t action =
  List.iter (process_action t action) ~f:(fun msg ->
    Faye.publish t.faye Channel.global msg)

let on_shape_active t ~f =
  t.on_shape_active <- f :: t.on_shape_active

