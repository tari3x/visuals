(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Std_internal

(* CR: try D3 *)

(* CR: bug if phone disconnects. *)

type t =
  { is_server : bool
  ; ctx : Ctx.t
  ; global : Atom.t Global.t
  ; touches : Multitouch.t
  ; mutable template : Atom.t Box.t
  ; mutable on_box_active : (Atom.t Box.t -> unit) list
  ; mutable transient_mode : bool
  ; mutable tracers : (float * Atom.t Box.t) list
  ; mutable most_recent : Box_id.t
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
    List.filter t.tracers ~f:(fun (time0, box) ->
      let alpha = (1. -. (time -. time0)) /. 5. in
      if Float.(alpha < 0.02) then false
      else begin
        let box = Box.set_alpha box ~alpha in
        Atom.render box t.ctx ~time;
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
  let time = Global.now_on_server t.global |> Time.to_sec in
  Ctx.clear t.ctx;
  Global.iter t.global ~f:(fun box ->
    Atom.render box t.ctx ~time);
  if t.is_server
  then begin
    render_tracers t ~time;
    render_help t;
  end;
  render_loop t

let touch_box t box_id box =
  (* CR: this shouldn't be updated on move. *)
  t.most_recent <- box_id;
  t.template <- box

let on_box_active t ~f =
  t.on_box_active <- f :: t.on_box_active

let call_on_box_active t box =
  List.iter t.on_box_active ~f:(fun f -> f box)

(* CR-someday: I'm not sure I have to actually wait for the window to load,
   but I suspect I do. *)
let create ctx ~is_server =
  let global_config =
    { Global.Config.
      viewport_width = Ctx.width ctx
    ; viewport_height = Ctx.height ctx
    ; is_server
    ; max_clients = 6
    ; max_box_age = Time.Span.of_sec 30.
    ; global_channel_name = "global"
    }
  in
  Global.create global_config ~sexp_of_a:Atom.sexp_of_t
  >>= fun global ->
  let t =
    { ctx
    ; global
    ; is_server
    (* Start with a bit of lies, never lie after that. *)
    ; most_recent = Box_id.create ()
    ; touches = Multitouch.create ()
    ; template = Box.default Atom.default
    ; on_box_active = []
    ; transient_mode = false
    ; tracers = []
    }
  in
  if t.is_server
  then Global.on_change t.global ~f:(fun _box_id box ->
    t.tracers <- (Time.now() |> Time.to_sec, box) :: t.tracers;);
  Lwt.async (fun () -> render_loop t);
  Lwt.return t

let apply_touch_update t (update : Multitouch.Update.t) =
  List.iter update ~f:(fun (box_id, update) ->
    match update with
    | None ->
      if t.transient_mode
      then Global.delete  t.global box_id
      else Global.release t.global box_id
    | Some update ->
      Global.change t.global box_id ~f:(fun box ->
        let box = Multitouch.Update.Single.apply update box in
        touch_box t box_id box;
        box))

let process_action t (action : Action.t) =
  match action.kind with
  | `move ->
    begin
      Multitouch.move t.touches action.changed_touches
      |> apply_touch_update t
    end
  | `down ->
    List.iter action.changed_touches ~f:(fun (touch : Action.Pointer.t) ->
      let box_id =
        Global.find t.global ~f:(fun box ->
          Atom.touched_by box touch.position)
      in
      match box_id with
      | Some box_id ->
        let box = Global.get_exn t.global box_id in
        Multitouch.add t.touches box_id (Box.frame box) touch
        |> apply_touch_update t;
        call_on_box_active t box;
        Global.request t.global box_id;
      | None ->
        match most_recent_active t with
        | Some (box_id, box) ->
          Multitouch.add t.touches box_id (Box.frame box) touch
          |> apply_touch_update t;
        | None ->
          let box = Box.set_translation t.template touch.position in
          let box_id = Global.add t.global box in
          Multitouch.add t.touches box_id (Box.frame box) touch
          |> apply_touch_update t)
  | `up ->
    Multitouch.remove t.touches action.changed_touches
    |> apply_touch_update t

let set_color t color_cycle =
  t.template <- Box.set t.template ~color_cycle;
  match most_recent_active t with
  | None -> ()
  | Some (box_id, box) ->
    let box = Box.set box ~color_cycle in
    Global.change t.global box_id ~f:(Fn.const box)

let set_shape t shape =
  t.template <- Box.set t.template ~kind:shape

let toggle_transient_mode t =
  t.transient_mode <- not t.transient_mode

