open Geometry
open Dom_wrappers

module Kind = struct
  type t =
  | Rectangle
  | Circle
  | Horizontal_line
  | Vertical_line
  | Cross_line
  | Bezier of Vector.t list

  let examples =
    [ Vertical_line
    ; Rectangle
    ; Horizontal_line
    ; Circle
    ; Cross_line
    (* CR: *)
    (* ; Bezier [] *)
    ]

  let to_string = function
    | Rectangle -> "Rectangle"
    | Circle -> "Circle"
    | Horizontal_line -> "Horz"
    | Vertical_line -> "Vert"
    | Cross_line -> "Cross"
    | Bezier _ -> "Bezier"

  let width = function
    | Rectangle       -> 200.
    | Circle          -> 200.
    | Horizontal_line -> 0.
    | Vertical_line   -> 0.
    | Cross_line      -> 0.
    | Bezier _        -> 0.

  let height = function
    | Rectangle       -> 200.
    | Circle          -> 200.
    | Horizontal_line -> 0.
    | Vertical_line   -> 0.
    | Cross_line      -> 0.
    | Bezier _        -> 0.

  let rect_width  = width Rectangle
  let rect_height = height Rectangle
  let circle_radius = width Circle /. 2.
  let line_width = 10.
  let line_touch_width = 60.

  let rec render t ctx =
    match t with
    | Rectangle ->
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
      (* CR: *)
    | Bezier _ -> ()

  let rec touched_by t p =
    let (x, y) = Vector.coords p in
    match t with
    | Rectangle ->
      abs_float x <= (rect_width /. 2.) && abs_float y <= (rect_height /. 2.)
    | Circle ->
      Vector.length p <= circle_radius
    | Horizontal_line ->
      abs_float x <= line_touch_width
    | Vertical_line ->
      abs_float y <= line_touch_width
    | Cross_line ->
      touched_by Horizontal_line p || touched_by Vertical_line p
    | Bezier _ -> false
end

open Kind

type t =
  { kind : Kind.t
  ; frame : Frame.t
  ; color : Color_cycle.t
  }

let frame t =
  t.frame

let add_frame t ~frame =
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
  create ~kind:Rectangle ~frame:Frame.ident ~color:Color_cycle.default

let render t ctx ~time =
  Ctx.save ctx;
  let current_color = Color_cycle.current_color t.color ~time in
  Ctx.set_fill_color ctx current_color;
  Ctx.transform ctx (Frame.matrix t.frame);
  Ctx.set_stroke_color ctx current_color;
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

(* CR: the same special casing needs to happen in [add_frame]. *)
let set_frame t ~frame =
  let frame =
    match t.kind with
    | Horizontal_line
    | Vertical_line
    | Cross_line ->
      Frame.remove_scale frame
    | Circle ->
      Frame.equal_scale frame
    | Rectangle -> frame
    | Bezier _ -> t.frame
  in
  { t with frame }

let set_touches t touches =
  let kind =
    match t.kind with
    | ( Horizontal_line
          | Vertical_line
          | Cross_line
          | Circle
          | Rectangle) as kind -> kind
    | Bezier _ -> Bezier touches
  in
  { t with kind }

let scale_to_fit t size =
  let current_size = max (width t.kind) (height t.kind) in
  let s = min 1.0 (size /. current_size) in
  let frame = Frame.(t.frame *> scale ~scale_x:s ~scale_y:s) in
  { t with frame }

module Local = struct
  type nonrec t =
    { shape : t
    ; scale : float
    }

  let to_string t =
    Printf.sprintf "{%s, scale = %f}"
      (to_string t.shape) t.scale
end

let to_local t ~viewport_scale =
  { Local. shape = t; scale = viewport_scale }

let of_local (local : Local.t) ~viewport_scale =
  let scale = viewport_scale /. local.scale in
  let t = local.shape in
  (* CR: scale bezier nodes. *)
  { t with frame = Frame.scale_viewport t.frame scale }
