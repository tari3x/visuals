open Common

module Channel = Faye.Channel
module Message = Faye.Message

type t =
  { ctx : Ctx.t
  ; canvas : Html.canvasElement Js.t
  ; faye : Faye.t
  ; is_leader : bool
  ; client_id : Client_id.t
  (* CR: only redraw shapes that have owners *)
  ; mutable shape_ids : Shape_id.t list
  ; shapes : (Shape_id.t, Shape.t) Hashtbl.t
  ; owners : (Shape_id.t, Client_id.t) Hashtbl.t
  ; mutable active : (Shape_id.t * Shape.t) option
  ; mutable pointer : Point.t
  }

let canvas t =
  t.canvas

let render t =
  Ctx.clear t.ctx;
  (* CR: better reverse in other places. *)
  List.iter (List.rev t.shape_ids) ~f:(fun shape_id ->
    let shape = Hashtbl.find t.shapes shape_id in
    Shape.render shape t.ctx)

let assign t client_id shape_id =
  let shape = Hashtbl.find t.shapes shape_id in
  Hashtbl.replace t.owners shape_id client_id;
  if t.client_id = client_id
  then t.active <- Some (shape_id, shape);
  t.shape_ids <- List.bring_to_front t.shape_ids shape_id

let release t shape_id =
  let client_id = Hashtbl.find t.owners shape_id in
  Hashtbl.remove t.owners shape_id;
  if t.client_id = client_id
  then t.active <- None

let process_message t = function
  | Message.Request (client_id, shape_id) ->
    if t.is_leader || not (Hashtbl.mem t.owners shape_id)
    then begin
      Hashtbl.replace t.owners shape_id client_id;
      Faye.publish t.faye Channel.global
        (Message.Grant (client_id, shape_id))
    end
  | Message.Release shape_id ->
    release t shape_id
  | Message.Grant (client_id, shape_id) ->
    assign t client_id shape_id
  | Message.Create (shape_id, client_id, shape) ->
    Hashtbl.replace t.shapes shape_id shape;
    assign t client_id shape_id
  | Message.Set (shape_id, shape) ->
    Hashtbl.replace t.shapes shape_id shape
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

let process_message t msg =
  process_message t msg;
  render t

(* CR-someday: I'm not sure I have to actually wait for the window to load,
   but I suspect I do. *)
let create ~is_leader =
  let id = "main_canvas" in
  let canvas = get_element_by_id id Html.CoerceTo.canvas in
  let ctx = canvas##getContext Html._2d_ in
  canvas##.width := Html.document##.body##.clientWidth;
  canvas##.height := Html.document##.body##.clientHeight;
  let faye = Faye.create ~url:"http://192.168.1.100:8000/faye" in
  let t =
    { canvas
    ; ctx
    ; faye
    ; is_leader
    ; client_id = Client_id.create ()
    ; shape_ids = []
    ; shapes = Hashtbl.create ()
    ; owners = Hashtbl.create ()
    ; active = None
    ; pointer = Point.create 0 0
    }
  in
  Faye.subscribe t.faye Channel.global ~f:(process_message t);
  let channel = Channel.create () in
  Faye.publish t.faye Channel.global (Message.Request_state channel);
  Faye.subscribe t.faye channel ~f:(process_message t);
  t

let action_to_messages t = function
  | Action.Move point ->
    let offset = Point.(point - t.pointer) in
    t.pointer <- point;
    begin match t.active with
    | None -> []
    | Some (shape_id, shape) ->
      let shape = Shape.move_by shape offset in
      t.active <- Some (shape_id, shape);
      [ Message.Set (shape_id, shape) ]
    end
  | Action.Down _ ->
    begin
      List.maybe_find t.shape_ids ~f:(fun shape_id ->
        let shape = Hashtbl.find t.shapes shape_id in
        Shape.touched_by shape t.pointer)
      |> function
        | Some shape_id ->
          [ Message.Request (t.client_id, shape_id) ]
        | None ->
          let shape_id = Shape_id.create () in
          let shape = Shape.create t.pointer in
          [ Message.Create (shape_id, t.client_id, shape) ]
    end
  | Action.Up _ ->
    match t.active with
    | None -> []
    | Some (shape_id, _) ->
      t.active <- None;
      [ Message.Release shape_id ]

let process_action t action =
  List.iter (action_to_messages t action) ~f:(fun msg ->
    Faye.publish t.faye Channel.global msg)
