(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Std_internal

type t =
  { grid : Grid.t
  ; global : Grid.Ctl.t Global.t
  }

let rec render_loop t =
  (* Lwt_js_events.request_animation_frame () *)
  Lwt_js.sleep 0.01
  >>= fun () ->
  Global.iter t.global ~f:(Grid.ctl t.grid);
  Grid.render t.grid;
  render_loop t

let start grid ~ctx =
  Global.create
    ~viewport_width:(Ctx.width ctx)
    ~viewport_height:(Ctx.height ctx)
    ~is_server:true
    ~max_clients:6
    ~sexp_of_a:Grid.Ctl.sexp_of_t
  >>= fun global ->
  let t = { grid; global } in
  Grid.render t.grid;
  Lwt.async (fun () -> render_loop t);
  Lwt.return ()
