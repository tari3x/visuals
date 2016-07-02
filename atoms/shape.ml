open Geometry
open Dom_wrappers

module Kind = struct
  type t =
  | Rect
  | Circle
  | Horizontal_line
  | Vertical_line
  | Cross_line

  let to_string = function
    | Rect -> "Rect"
    | Circle -> "Circle"
    | Horizontal_line -> "Horz"
    | Vertical_line -> "Vert"
    | Cross_line -> "Cross"

  let rect_width  = 200.
  let rect_height = 200.
  let circle_radius = 100.
  let line_width = 10.
  let line_touch_width = 60.

  let rec render t ctx =
    match t with
    | Rect ->
      Ctx.draw_centered_rectangle ctx ~width:rect_width ~height:rect_height
    | Circle ->
      Ctx.draw_circle ctx Vector.zero ~radius:circle_radius
    | Horizontal_line ->
      Ctx.draw_horizontal_line ctx Vector.zero ~width:line_width
    | Vertical_line ->
      Ctx.draw_vertical_line ctx Vector.zero ~width:line_width
    | Cross_line ->
      render Horizontal_line ctx;
      render Vertical_line ctx

  let rec touched_by t p =
    let (x, y) = Vector.coords p in
    match t with
    | Rect ->
      abs_float x <= (rect_width /. 2.) && abs_float y <= (rect_height /. 2.)
    | Circle ->
      Vector.length p <= circle_radius
    | Horizontal_line ->
      abs_float x <= line_touch_width
    | Vertical_line ->
      abs_float y <= line_touch_width
    | Cross_line ->
      touched_by Horizontal_line p || touched_by Vertical_line p
end

open Kind

type t =
  { kind : Kind.t
  ; frame : Frame.t
  ; color : Color_cycle.t
  }

let frame t =
  t.frame

let add_frame t frame =
  { t with frame = Frame.(t.frame *> frame) }

let color t =
  t.color

let to_string t =
  let { kind; frame; color } = t in
  Printf.sprintf "{ %s; %s; %s }"
    (Kind.to_string kind)
    (Frame.to_string frame)
    (Color_cycle.to_string color)

let create ~kind ~frame ~color =
  { kind; frame; color }

let default =
  create ~kind:Rect ~frame:Frame.ident ~color:Color_cycle.default

let render t ctx ~time ~transform =
  Ctx.save ctx;
  Ctx.set_fill_color ctx (Color_cycle.current_color t.color ~time);
  Ctx.set_stroke_color ctx (Color_cycle.current_color t.color ~time);
  let transform = Matrix.(Frame.matrix t.frame *> transform) in
  Ctx.transform ctx transform;
  Kind.render t.kind ctx;
  Ctx.restore ctx

let touched_by t p =
  let m_inv = Matrix.inv (Frame.matrix t.frame) in
  let p = Matrix.apply m_inv p in
  Kind.touched_by t.kind p

let set_translation t v =
  { t with frame = Frame.set_translation t.frame v }

let set_alpha t ~alpha =
  { t with color = Color_cycle.set_alpha t.color ~alpha }

let set_frame t frame =
  let frame =
    match t.kind with
    | Horizontal_line
    | Vertical_line
    | Cross_line ->
      Frame.remove_scale frame
    | Circle ->
      Frame.equal_scale frame
    | Rect -> frame
  in
  { t with frame }

