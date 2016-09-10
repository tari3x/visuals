open Lwt
open Common
open Dom_wrappers

type t =
  { ctx : Ctx.t
  ; global : Global.t
  ; touches : Multitouch.t
  ; mutable shape : Shape.t
  }

let create ctx shape =
  Global.create
    ~viewport_width:(Ctx.width ctx)
    ~viewport_height:(Ctx.height ctx)
    ~is_server:false
    ~max_clients:2
  >>= fun global ->
  Lwt.return { ctx
             ; global
             ; touches = Multitouch.create ()
             ; shape
             }

let apply_touch_update t (update : Multitouch.Update.t) =
  List.iter update ~f:(fun (shape_id, update) ->
    match update with
    | None -> Global.delete t.global shape_id
    | Some update ->
      Global.change t.global shape_id ~f:(fun shape ->
        let shape = Multitouch.Update.Single.apply update shape in
        t.shape <- shape;
        shape))

let process_action t (action : Action.t) =
  (* debug "Action: %s" (Action.to_string action); *)
  match action.kind with
  | `move ->
    begin
      Multitouch.move t.touches action.changed_touches
       |> apply_touch_update t
    end
  | `down ->
    let touch = List.hd action.changed_touches in
    begin match Multitouch.active t.touches with
    | [] ->
      let shape = Shape.set_translation t.shape touch.position in
      let shape_id = Global.add t.global shape in
      Multitouch.add t.touches shape_id (Shape.frame shape) touch
      |> apply_touch_update t
    | shape_ids ->
      List.iter shape_ids ~f:(fun shape_id ->
        let shape = Global.get_exn t.global shape_id in
        (* CR-someday: this is a bit weird. *)
        Multitouch.add t.touches shape_id (Shape.frame shape) touch
        |> apply_touch_update t)
    end
  | `up ->
    Multitouch.remove t.touches action.changed_touches
    |> apply_touch_update t

let set_color t color =
  t.shape <- Shape.set t.shape ~color;
  List.iter (Multitouch.active t.touches) ~f:(fun shape_id ->
    Global.change t.global shape_id ~f:(Shape.set ~color))
