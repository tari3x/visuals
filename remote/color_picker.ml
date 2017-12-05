(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Util
open Common
open Dom_wrappers
open Geometry

let black_strip_width =
  if Config.drawing_mode
  then 30.
  else 0.

let colors =
  [ Color.red; Color.magenta; Color.blue
  ; Color.cyan; Color.green; Color.yellow
  ; Color.red ]

let color_for_coordinate =
  let open Float in
  fun ~width ~height v ->
    let (x, y) = Vector.coords v in
    let color = Color.interpolate colors (y / height) in
    let scaling_width = width * 0.5 in
    if x < black_strip_width
    then Color.black
    else if x > scaling_width
    then color
    else Color.scale color (x / scaling_width)

let draw ctx =
  let width = Ctx.width ctx in
  let height = Ctx.height ctx in
  let cell_size = 3 in
  let cell_size_float = float cell_size in
  for i = 0 to (int width) do
    for j = 0 to (int height) do
      if Caml.(i mod cell_size) = 0 && Caml.(j mod cell_size) = 0
      then begin
        let v = Vector.create i j in
        let color = color_for_coordinate v ~width ~height in
        Ctx.set_fill_color ctx color;
        (* CR: try direct pixel manipulation *)
        Ctx.fill_rect ctx v ~width:cell_size_float ~height:cell_size_float
      end
    done
  done

let run t ctx =
  let width = Ctx.width ctx in
  let height = Ctx.height ctx in
  Ctx.canvas_actions ctx
  |> Lwt_stream.iter_with_try ~f:(fun (action : Action.t) ->
    match action.kind, action.button with
    | `move, `touch
    | `down, _ ->
      let v = Action.coords action in
      let color = color_for_coordinate v ~width ~height in
      State_light.set_color t color
    | _ -> ())
