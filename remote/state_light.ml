(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Std_internal

type 'a t =
  { ctx : Ctx.t
  ; global : 'a Global.t
  ; touches : Multitouch.t
  ; mutable shape : 'a Box.t
  }

let create ctx shape ~sexp_of_a =
  Global.create
    ~viewport_width:(Ctx.width ctx)
    ~viewport_height:(Ctx.height ctx)
    ~is_server:false
    ~max_clients:2
    ~sexp_of_a
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
    let touch = List.hd_exn action.changed_touches in
    begin match Multitouch.active t.touches with
    | [] ->
      let shape = Box.set_translation t.shape touch.position in
      let shape_id = Global.add t.global shape in
      Multitouch.add t.touches shape_id (Box.frame shape) touch
      |> apply_touch_update t
    | shape_ids ->
      List.iter shape_ids ~f:(fun shape_id ->
        let shape = Global.get_exn t.global shape_id in
        (* CR-someday: this is a bit weird. *)
        Multitouch.add t.touches shape_id (Box.frame shape) touch
        |> apply_touch_update t)
    end
  | `up ->
    Multitouch.remove t.touches action.changed_touches
    |> apply_touch_update t

let set_color t color =
  let color_cycle = Color_cycle.const color in
  t.shape <- Box.set t.shape ~color_cycle;
  List.iter (Multitouch.active t.touches) ~f:(fun shape_id ->
    Global.change t.global shape_id ~f:(Box.set ~color_cycle))
