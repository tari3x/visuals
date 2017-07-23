(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Common
open Dom_wrappers

(* CR: try D3 *)

(* CR: bug if phone disconnects. *)

type t =
  { is_server : bool
  ; ctx : Ctx.t
  ; global : Global.t
  ; touches : Multitouch.t
  ; mutable template : Shape.t
  ; mutable on_shape_active : (Shape.t -> unit) list
  ; mutable transient_mode : bool
  ; mutable tracers : (float * Shape.t) list
  ; mutable most_recent : Shape_id.t
  }

(* CR: you want to not necessarily hold this for getting the color. *)
let most_recent t =
  Global.get t.global t.most_recent

(* CR: use [Multitouch.most_recent_active]. *)
let most_recent_active t =
  if Multitouch.is_touching t.touches t.most_recent
  then Some (t.most_recent, Global.get_exn t.global t.most_recent)
  else None

let render_tracers t ~time =
  t.tracers <-
    List.filter t.tracers ~f:(fun (time0, shape) ->
      let alpha = (1. -. (time -. time0)) /. 5. in
      if Float.(alpha < 0.02) then false
      else begin
        let shape = Shape.set_alpha shape ~alpha in
        Shape.render shape t.ctx ~time;
        true
      end)

let render_help t =
  let x = Ctx.width t.ctx -. 200. in
  Ctx.set_fill_color t.ctx Color.green;
  Ctx.set_font t.ctx "20px Arial";
  (* Ctx.fill_text t.ctx "join wifi: atoms" x 20.; *)
  (* 50 for the second line *)
  Ctx.fill_text t.ctx current_url x 20.

let rec render_loop t =
  Lwt_js.sleep 0.1
  >>= fun () ->
  let time = Global.now_on_server t.global |> Time.to_seconds in
  Ctx.clear t.ctx;
  Global.iter t.global ~f:(fun shape ->
    Shape.render shape t.ctx ~time);
  if t.is_server
  then begin
    render_tracers t ~time;
    render_help t;
  end;
  render_loop t

let touch_shape t shape_id shape =
  (* CR: this shouldn't be updated on move. *)
  t.most_recent <- shape_id;
  t.template <- shape

let on_shape_active t ~f =
  t.on_shape_active <- f :: t.on_shape_active

let call_on_shape_active t shape =
  List.iter t.on_shape_active ~f:(fun f -> f shape)

(* CR-someday: I'm not sure I have to actually wait for the window to load,
   but I suspect I do. *)
let create ctx ~is_server =
  Global.create
    ~viewport_width:(Ctx.width ctx)
    ~viewport_height:(Ctx.height ctx)
    ~is_server
    ~max_clients:6
  >>= fun global ->
  let t =
    { ctx
    ; global
    ; is_server
    (* Start with a bit of lies, never lie after that. *)
    ; most_recent = Shape_id.create ()
    ; touches = Multitouch.create ()
    ; template = Shape.default
    ; on_shape_active = []
    ; transient_mode = false
    ; tracers = []
    }
  in
  if t.is_server
  then Global.on_change t.global ~f:(fun _shape_id shape ->
    t.tracers <- (Time.now() |> Time.to_seconds, shape) :: t.tracers;);
  Lwt.async (fun () -> render_loop t);
  Lwt.return t

let apply_touch_update t (update : Multitouch.Update.t) =
  List.iter update ~f:(fun (shape_id, update) ->
    match update with
    | None ->
      if t.transient_mode
      then Global.delete  t.global shape_id
      else Global.release t.global shape_id
    | Some update ->
      Global.change t.global shape_id ~f:(fun shape ->
        let shape = Multitouch.Update.Single.apply update shape in
        touch_shape t shape_id shape;
        shape))

let process_action t (action : Action.t) =
  match action.kind with
  | `move ->
    begin
      Multitouch.move t.touches action.changed_touches
      |> apply_touch_update t
    end
  | `down ->
    List.iter action.changed_touches ~f:(fun (touch : Action.Pointer.t) ->
      let shape_id =
        Global.find t.global ~f:(fun shape ->
          Shape.touched_by shape touch.position)
      in
      match shape_id with
      | Some shape_id ->
        let shape = Global.get_exn t.global shape_id in
        Multitouch.add t.touches shape_id (Shape.frame shape) touch
        |> apply_touch_update t;
        call_on_shape_active t shape;
        Global.request t.global shape_id;
      | None ->
        match most_recent_active t with
        | Some (shape_id, shape) ->
          Multitouch.add t.touches shape_id (Shape.frame shape) touch
          |> apply_touch_update t;
        | None ->
          let shape = Shape.set_translation t.template touch.position in
          let shape_id = Global.add t.global shape in
          Multitouch.add t.touches shape_id (Shape.frame shape) touch
          |> apply_touch_update t)
  | `up ->
    Multitouch.remove t.touches action.changed_touches
    |> apply_touch_update t

let set_color t color =
  t.template <- Shape.set t.template ~color;
  match most_recent_active t with
  | None -> ()
  | Some (shape_id, shape) ->
    let shape = Shape.set shape ~color in
    Global.change t.global shape_id ~f:(Fn.const shape)

let set_shape_kind t kind =
  t.template <- Shape.set t.template ~kind

let toggle_transient_mode t =
  t.transient_mode <- not t.transient_mode

