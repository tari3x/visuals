open Lwt
open Common
open Geometry
open Dom_wrappers

module Channel = Faye.Channel
module Message = Faye.Message

(* CR: try D3 *)

(* CR: bug if phone disconnects. *)

type t =
  { ctx : Ctx.t
  ; faye : Faye.t
  ; is_server : bool
  ; client_id : Client_id.t
  (* CR: only redraw shapes that have owners *)
  ; mutable shape_ids : Shape_id.t list
  ; shapes : (Shape_id.t, Shape.t) Hashtbl.t
  ; owners : (Shape_id.t, Client_id.t) Hashtbl.t
  ; mutable last_touched : Shape_id.t
  ; touches : Multitouch.t
  ; mutable template : Shape.t
  ; mutable on_shape_active : (Shape.t -> unit) list
  (* This is synchronized with the master for two purposes. First, so that
     loops look somewhat synchronous on all devices. Second, to make it more
     likely that times are positive in all computations. *)
  ; mutable time_offset : float
  (* Transformation from the server coordinate system into ours. *)
  ; mutable viewport : Matrix.t
  ; mutable transient_mode : bool
  ; mutable tracers : (float * Shape.t) list
  }

let last_touched t =
  Hashtbl.maybe_find t.shapes t.last_touched

let last_touched_still_touching t =
  if Multitouch.is_touching t.touches t.last_touched
  then Some (t.last_touched, Hashtbl.find t.shapes t.last_touched)
  else None

let render_tracers t ~time =
  t.tracers <-
    List.filter t.tracers ~f:(fun (time0, shape) ->
      let alpha = (1. -. (time -. time0)) /. 5. in
      if alpha < 0.02 then false
      else begin
        let shape = Shape.set_alpha shape ~alpha in
        Shape.render shape t.ctx ~time ~transform:t.viewport;
        true
      end)

let rec render_loop t =
  Lwt_js.sleep 0.1
  >>= fun () ->
  let time = Unix.gettimeofday () +. t.time_offset in
  Ctx.clear t.ctx;
  (* CR: better reverse in other places. *)
  List.iter (List.rev t.shape_ids) ~f:(fun shape_id ->
    match Hashtbl.maybe_find t.shapes shape_id with
    | None ->
      (* CR: does this happen, and why? *)
      error "shape not found while rendering";
    | Some shape ->
      Shape.render shape t.ctx ~time ~transform:t.viewport);
  if t.is_server then render_tracers t ~time;
  render_loop t

let is_owner t shape_id =
  match Hashtbl.maybe_find t.owners shape_id with
  | None -> false
  | Some client_id -> client_id = t.client_id

let release_locally t shape_id =
  Hashtbl.remove t.owners shape_id

let release_globally t shape_id =
  Faye.publish t.faye Channel.global (Message.Release shape_id)

let assign t client_id shape_id =
  (* Could be a very old message. *)
  match Hashtbl.maybe_find t.shapes shape_id with
  | None ->
    if t.client_id = client_id then release_globally t shape_id
  | Some shape ->
    Hashtbl.replace t.owners ~key:shape_id ~data:client_id;
    (* This adds new shapes to [shape_ids]. *)
    t.shape_ids <- List.bring_to_front t.shape_ids shape_id;
    if t.client_id = client_id
    then begin
      if not (Multitouch.is_touching t.touches shape_id)
      then release_globally t shape_id
      else begin
        let color = t.template.color in
        t.template <- { shape with color};
        List.iter t.on_shape_active ~f:(fun f -> f shape)
      end
    end

let touch_shape t shape_id shape =
  t.last_touched <- shape_id;
  t.template <- shape

let init_message t =
  let shapes =
    List.map (List.rev t.shape_ids) ~f:(fun shape_id ->
      let shape = Hashtbl.find t.shapes shape_id in
      let client_id = Hashtbl.maybe_find t.owners shape_id in
      (shape_id, shape, client_id))
  in
  Message.Init
    { shapes
    ; width = Ctx.width t.ctx
    ; height = Ctx.height t.ctx
    ; time = Unix.gettimeofday ()
    }

let init t msg =
  let { Message.Init. shapes; width; height; time } = msg in
  List.iter shapes ~f:(fun (shape_id, shape, client_id) ->
    t.shape_ids <- shape_id :: t.shape_ids;
    Hashtbl.add t.shapes shape_id shape;
    Option.iter client_id ~f:(fun client_id ->
      Hashtbl.add t.owners shape_id client_id));
  t.shape_ids <- List.rev t.shape_ids;
  t.time_offset <- time -. Unix.gettimeofday ();
  let scale_x = (Ctx.width t.ctx) /. width in
  let scale_y = (Ctx.height t.ctx) /. height in
  let scale = min (min scale_x scale_y) 1.0 in
  t.viewport <- Matrix.scale ~scale_x:scale ~scale_y:scale

let process_message t = function
  | Message.Request (client_id, shape_id) ->
    if t.is_server && not (Hashtbl.mem t.owners shape_id)
    then begin
      Hashtbl.replace t.owners ~key:shape_id ~data:client_id;
      Faye.publish t.faye Channel.global
        (Message.Grant (client_id, shape_id))
    end
  | Message.Release shape_id ->
    release_locally t shape_id
  | Message.Grant (client_id, shape_id) ->
    assign t client_id shape_id
  | Message.Create (shape_id, client_id, shape) ->
    Hashtbl.replace t.shapes ~key:shape_id ~data:shape;
    assign t client_id shape_id
  | Message.Set (shape_id, shape) ->
    Hashtbl.replace t.shapes ~key:shape_id ~data:shape;
    if t.is_server
    then t.tracers <- (Unix.gettimeofday (), shape) :: t.tracers;
  | Message.Delete shape_id ->
    Hashtbl.remove t.shapes shape_id;
    t.shape_ids <- List.delete t.shape_ids shape_id;
    release_locally t shape_id
  | Message.Request_init channel ->
    if t.is_server then Faye.publish t.faye channel (init_message t)
  | Message.Init msg ->
    if not t.is_server then init t msg

(* CR-someday: I'm not sure I have to actually wait for the window to load,
   but I suspect I do. *)
let create ctx ~is_server =
  let faye = Faye.create () in
  let t =
    { ctx
    ; faye
    ; is_server
    ; client_id = Client_id.create ()
    ; shape_ids = []
    ; shapes = Hashtbl.create ()
    ; owners = Hashtbl.create ()
    (* Start with a bit of lies, never lie after that. *)
    ; last_touched = Shape_id.create ()
    ; touches = Multitouch.create ()
    ; template = Shape.default
    ; on_shape_active = []
    ; time_offset = 0.
    ; viewport = Matrix.ident
    ; transient_mode = false
    ; tracers = []
    }
  in
  Faye.subscribe t.faye Channel.global ~f:(process_message t);
  let channel = Channel.create () in
  Faye.publish t.faye Channel.global (Message.Request_init channel);
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
        touch_shape t shape_id shape;
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
        touch_shape t shape_id shape;
        [ Message.Request (t.client_id, shape_id) ]
      | None ->
        match last_touched_still_touching t with
        | Some (shape_id, shape) ->
          Multitouch.add t.touches shape_id (Shape.frame shape) touch;
          []
        | None ->
          let shape_id = Shape_id.create () in
          let shape = Shape.set_translation t.template touch.position in
          Multitouch.add t.touches shape_id (Shape.frame shape) touch;
          touch_shape t shape_id shape;
          [ Message.Create (shape_id, t.client_id, shape) ]
    )
  | `up ->
    Multitouch.remove t.touches action.changed_touches
    |> List.map ~f:(fun shape_id ->
      if t.transient_mode
      then Message.Delete  shape_id
      else Message.Release shape_id)

let process_action t = function
  | Action.Pointer action -> process_pointer_action t action
  | Action.Set_color color ->
    t.template <- { t.template with color };
    match last_touched t with
    | None -> []
    | Some shape ->
      let shape = { shape with color } in
      (* CR: bypassing ownership check becuase something is fucked up.

         I think it's fixed now, check later.
      *)
      [ Message.Set (t.last_touched, shape) ]
    (*
    match last_touched_still_touching t with
    | None ->
      []
    | Some (shape_id, shape) ->
      let shape = { shape with color } in
      (* CR: bypassing ownership check becuase something is fucked up. *)
      (* set_shape t t.last_touched shape *)
      [ Message.Set (shape_id, shape) ]
    *)

let process_action t action =
  let action = Action.transform_positions (Matrix.inv t.viewport) action in
  List.iter (process_action t action) ~f:(fun msg ->
    Faye.publish t.faye Channel.global msg)

let on_shape_active t ~f =
  t.on_shape_active <- f :: t.on_shape_active

let set_shape_kind t kind =
  t.template <- { t.template with kind }

let toggle_transient_mode t =
  t.transient_mode <- not t.transient_mode

