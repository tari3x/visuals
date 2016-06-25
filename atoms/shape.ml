open Geometry
open Dom_wrappers

module Kind = struct
  type t =
  | Rect

  let to_string = function
    | Rect -> "Rect"

  let rect_width  = 100.
  let rect_height = 100.

  let render t ctx =
    match t with
    | Rect ->
      Ctx.draw_centered_rectangle ctx ~width:rect_width ~height:rect_height

  let touched_by t p =
    let (x, y) = Vector.coords p in
    match t with
    | Rect ->
      abs_float x <= (rect_width /. 2.) && abs_float y <= (rect_height /. 2.)
end

open Kind

type t =
  { kind : Kind.t
  ; frame : Frame.t
  ; color : Color_cycle.t
  }

let frame t =
  t.frame

let set_frame t frame =
  { t with frame }

let color t =
  t.color

let set_color t color =
  { t with color }

let to_string t =
  let { kind; frame; color } = t in
  Printf.sprintf "{ %s; %s; %s }"
    (Kind.to_string kind)
    (Frame.to_string frame)
    (Color_cycle.to_string color)

let create anchor color =
  let frame =
    Frame.(
      translate anchor +> rotate (Angle.of_degrees 45.))
  in
  let kind = Rect in
  { kind; frame; color }

let dummy =
  create Vector.zero Color_cycle.default

let render t ctx ~time =
  Ctx.save ctx;
  Ctx.set_fill_color ctx (Color_cycle.current_color t.color ~time);
  Ctx.transform ctx (Frame.matrix t.frame);
  Kind.render t.kind ctx;
  Ctx.restore ctx

let touched_by t p =
  let m_inv = Matrix.inv (Frame.matrix t.frame) in
  let p = Matrix.apply m_inv p in
  Kind.touched_by t.kind p
