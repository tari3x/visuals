open Common

module Kind = struct
  type t =
  | Rect of (float * float)

  let to_string = function
    | Rect (w, h) ->
      Printf.sprintf "Rect (%f, %f)" w h
end

open Kind

type t =
  { kind : Kind.t
  ; anchor : Point.t
  ; color : Color.t
  }

let to_string t =
  let { kind; anchor; color } = t in
  Printf.sprintf "{ %s; %s; %s }"
    (Kind.to_string kind)
    (Point.to_string anchor)
    (Color.to_string color)

let create anchor =
  let kind = Rect (200., 100.) in
  let color = Color.random () in
  { kind; anchor; color }

let render t ctx =
  Ctx.set_fill_color ctx t.color;
  match t.kind with
  | Rect (width, height) ->
    Ctx.draw_rectangle ctx t.anchor width height

let move_by t p =
  let anchor = Point.(t.anchor + p) in
  { t with anchor }

let touched_by t p =
  match t.kind with
  | Rect (width, height) ->
    let anchor_x, anchor_y = Point.coords t.anchor in
    let x, y = Point.coords p in
    anchor_x <= x && x <= anchor_x +. width
    && anchor_y <= y && y <= anchor_y +. height
