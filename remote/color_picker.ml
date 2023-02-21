(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

module Config = struct
  type t =
    (* included in drawing mode *)
    { include_black_strip : bool
    } [@@deriving sexp]
end

type t =
  { config : Config.t
  ; ctx : Ctx.t
  }

let create config ctx =
  { config; ctx }

let colors =
  [ Color.red; Color.magenta; Color.blue
  ; Color.cyan; Color.green; Color.yellow
  ; Color.red ]

let color_for_coordinate t =
  let open Float in
  let black_strip_width =
    if t.config.include_black_strip
    then 30.
    else 0.
  in
  fun ~width ~height v ->
    let (x, y) = Vector.coords v in
    let color = Color.interpolate colors ~arg:(y / height) in
    let scaling_width = width * 0.5 in
    if x < black_strip_width
    then Color.black
    else if x > scaling_width
    then color
    else Color.scale color ~by:(x / scaling_width)

let draw t =
  let width = Ctx.width t.ctx in
  let height = Ctx.height t.ctx in
  let cell_size = 3 in
  let cell_size_float = float cell_size in
  let color_for_coordinate = color_for_coordinate t in
  for i = 0 to (int width) do
    for j = 0 to (int height) do
      if Caml.(i mod cell_size) = 0 && Caml.(j mod cell_size) = 0
      then begin
        let v = Vector.create i j in
        let color = color_for_coordinate v ~width ~height in
        Ctx.set_fill_color t.ctx color;
        (* CR: try direct pixel manipulation *)
        Ctx.fill_rect t.ctx v ~width:cell_size_float ~height:cell_size_float
      end
    done
  done

let run t state =
  let width = Ctx.width t.ctx in
  let height = Ctx.height t.ctx in
  let color_for_coordinate = color_for_coordinate t in
  Ctx.canvas_actions t.ctx
  |> Lwt_stream.iter_with_try ~f:(fun (action : Action.t) ->
    match action.kind, action.button with
    | `move, `touch
    | `down, _ ->
      let v = Action.coords action in
      let color = color_for_coordinate v ~width ~height in
      State_light.set_color state color
    | _ -> ())
