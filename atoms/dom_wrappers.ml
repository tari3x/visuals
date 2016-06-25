open Js
open Common
open Geometry
open Action

module Button = struct
  type t = [ `left | `right | `middle | `touch | `none ]
end

module Mouse_event = struct
  type t = Dom_html.mouseEvent Js.t

  let client_coords t =
    Vector.create t##.clientX t##.clientY

  let button t : Button.t =
    match Html.buttonPressed t with
    | Html.No_button     -> `none
    | Html.Left_button   -> `left
    | Html.Middle_button -> `middle
    | Html.Right_button  -> `right

  let action (t : t) kind =
    let position = client_coords t in
    let button   = button t in
    let id = 1 in
    let changed_touches =
      [ { Pointer. id; position; button } ]
    in
    Action.Pointer { kind; changed_touches }
end

(* CR: make it work with multi-touch. *)
module Touch_event = struct
  type t = Dom_html.touchEvent Js.t

  let action (t : t) kind =
    let changed_touches = t##.changedTouches in
    let changed_touches =
      List.init (changed_touches##.length) ~f:(fun i -> changed_touches##item i)
      |> List.map ~f:(fun touch ->
        let touch = Optdef.value_exn touch in
        let position = Vector.create touch##.clientX touch##.clientY in
        let id = touch##.identifier in
        { Pointer. id; position; button = `touch })
    in
    Action.Pointer { kind; changed_touches }
end

module Ctx = struct
  type t = Html.canvasRenderingContext2D Js.t

  let clear t =
    let width = t##.canvas##.clientWidth |> float in
    let height = t##.canvas##.clientHeight |> float in
    t##clearRect 0. 0. width height

  let draw_circle (t : t) p ~radius =
    t##beginPath;
    t##arc (Vector.x p) (Vector.y p) radius 0. (2. *. Js.math##._PI) _false;
    t##fill;
    (* CR: is this necessary? *)
    t##beginPath

  let draw_horizontal_line (t : t) p ~width =
    let y = Vector.y p in
    let top = y -. width /. 2. in
    let screen_width = float (t##.canvas##.clientWidth) in
    t##fillRect 0. top screen_width width

  let draw_vertical_line (t : t) p ~width =
    let x = Vector.x p in
    let left = x -. width /. 2. in
    let screen_height = float (t##.canvas##.clientHeight) in
    t##fillRect left 0. width screen_height

  let draw_centered_rectangle (t : t) ~width ~height =
    t##fillRect
      ((-.width) /. 2.)
      ((-.height) /. 2.)
      width height

  let set_fill_color (t : t) color =
    t##.fillStyle := string (Color.to_string color)

  let save (t : t) =
    t##save

  let restore (t : t) =
    t##restore

  let transform (t : t) m =
    let m = Matrix.coeffs m in
    let c i j = m.(i).(j) in
    t##transform
      (c 0 0) (c 1 0)
      (c 0 1) (c 1 1)
      (c 0 2) (c 1 2)
end

