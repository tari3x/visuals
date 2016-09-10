open! Common
open Geometry
open Dom_wrappers

let default_line_width = 40.

let rect_width  = 200.
let rect_height = 200.
let circle_radius = 100.
let line_touch_width = 60.

module Kind = struct
  type t =
  | Rectangle
  | Circle
  | Horizontal_line
  | Vertical_line
  | Cross_line
  | Zigzag of Vector.t list
  | Bezier of Vector.t list

  let examples =
    let p1 = Vector.create (-50) (50) in
    let p2 = Vector.create 0   (-50) in
    let p3 = Vector.create 50  50 in
    [ Zigzag [ p1; p2; p3 ]
    ; Vertical_line
    ; Rectangle
    ; Horizontal_line
    ; Circle
    ; Cross_line
    ; Bezier [ p1; p2; p3 ]
    ]

  let to_string = function
    | Rectangle -> "Rectangle"
    | Circle -> "Circle"
    | Horizontal_line -> "Horz"
    | Vertical_line -> "Vert"
    | Cross_line -> "Cross"
    | Zigzag _ -> "Zigzag"
    | Bezier _ -> "Bezier"

  let max_diff vs ~f =
    match List.map vs ~f with
    | [] -> 0.
    | x :: xs ->
      let max = List.fold_left xs ~init:x ~f:max in
      let min = List.fold_left xs ~init:x ~f:min in
      max -. min

  let width ~line_width = function
    | Rectangle       -> rect_width
    | Circle          -> circle_radius *. 2.
    | Horizontal_line -> 0.
    | Vertical_line   -> 0.
    | Cross_line      -> 0.
    | Zigzag vs       -> max_diff vs ~f:Vector.x +. (line_width *. 2.)
    | Bezier vs       -> max_diff vs ~f:Vector.x +. (line_width *. 2.)

  let height ~line_width = function
    | Rectangle       -> rect_height
    | Circle          -> circle_radius *. 2.
    | Horizontal_line -> 0.
    | Vertical_line   -> 0.
    | Cross_line      -> 0.
    | Zigzag vs       -> max_diff vs ~f:Vector.y +. (line_width *. 2.)
    | Bezier vs       -> max_diff vs ~f:Vector.y +. (line_width *. 2.)

  let render_zigzag ctx vs =
    let rec loop = function
      | [] -> ()
      | v :: vs ->
        Ctx.line_to ctx v;
        loop vs
    in
    match vs with
    | [] -> ()
    | v :: vs ->
      Ctx.save ctx;
      Ctx.begin_path ctx;
      Ctx.move_to ctx v;
      loop vs;
      Ctx.stroke_without_transform ctx;
      Ctx.restore ctx

  let render_bezier ctx vs =
    let make_cp v1 v2 v3 =
      let v = Vector.((v3 - v1) / 2.) in
      Vector.[ v2 - v; v2 + v ]
    in
    let rec control_points = function
      | [] | [ _ ] -> []
      | [ _; v2 ] -> [ v2 ]
      | v1 :: v2 :: v3 :: vs ->
        make_cp v1 v2 v3 @ control_points (v2 :: v3 :: vs)
    in
    let rec loop vs ~control:cs =
      match vs, cs with
      | [], [] -> ()
      | v :: vs, c1 :: c2 :: cs ->
        Ctx.bezier_curve_to ctx ~control1:c1 ~control2:c2 v;
        loop vs ~control:cs
      | _ ->
        failwith "BUG in render_bezier"
    in
    match vs with
    | [] | [ _ ] | [ _; _ ] -> render_zigzag ctx vs
    | v :: vs ->
      let control = v :: control_points (v :: vs) in
      Ctx.save ctx;
      Ctx.begin_path ctx;
      Ctx.move_to ctx v;
      loop vs ~control;
      Ctx.stroke_without_transform ctx;
      Ctx.restore ctx

  let rec render t ctx =
    match t with
    | Rectangle ->
      Ctx.draw_centered_rectangle ctx ~width:rect_width ~height:rect_height
    | Circle ->
      Ctx.draw_circle ctx Vector.zero ~radius:circle_radius
    | Horizontal_line ->
      Ctx.draw_horizontal_line ctx Vector.zero
    | Vertical_line ->
      Ctx.draw_vertical_line ctx Vector.zero
    | Cross_line ->
      render Horizontal_line ctx;
      render Vertical_line ctx
    | Zigzag vs -> render_zigzag ctx vs
    | Bezier vs -> render_bezier ctx vs

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
    | Zigzag _ -> false
    | Bezier _ -> false
end

open Kind

module T : sig
  type t

  val create
    :  kind:Kind.t
    -> frame:Frame.t
    -> color:Color_cycle.t
    -> ?line_width:float
    -> unit
    -> t

  val frame : t -> Frame.t
  val color : t -> Color_cycle.t
  val kind : t -> Kind.t
  val line_width : t -> float

  val set
    :  ?kind:Kind.t
    -> ?frame:Frame.t
    -> ?color:Color_cycle.t
    -> ?line_width:float
    -> t
    -> t

  val to_string : t -> string
end = struct
  type t =
    { kind : Kind.t
    ; frame : Frame.t
    ; color : Color_cycle.t
    ; line_width : float
    }

  let create ~kind ~frame ~color ?(line_width = default_line_width) () =
    { kind; frame; color; line_width }

  let frame t = t.frame
  let color t = t.color
  let kind t = t.kind
  let line_width t = t.line_width

  let set t
      ?(kind = t.kind)
      ?(frame = t.frame)
      ?(color = t.color)
      ?(line_width = t.line_width)
      () =
    let frame =
      match t.kind with
      | Circle ->
        Frame.equal_scale frame
      | Horizontal_line
      | Vertical_line
      | Cross_line
      | Zigzag _
      | Bezier _
      | Rectangle -> frame
    in
    { kind; frame; color; line_width }

  let set ?kind ?frame ?color ?line_width t =
    set ?kind ?frame ?color ?line_width t ()

  let to_string t =
    let { kind; frame; color; line_width } = t in
    Printf.sprintf "{ %s; %s; %s; %f }"
      (Kind.to_string kind)
      (Frame.to_string frame)
      (Color_cycle.to_string color)
      line_width
end

include T

let default =
  create
    ~kind:Rectangle
    ~frame:Frame.ident
    ~color:Color_cycle.default
    ()

let render t ctx ~time =
  Ctx.save ctx;
  let current_color = Color_cycle.current_color (color t) ~time in
  Ctx.set_fill_color ctx current_color;
  Ctx.set_stroke_color ctx current_color;
  Ctx.set_line_width ctx (line_width t);
  Ctx.set_line_join ctx `round;
  Ctx.set_line_cap ctx `round;
  Ctx.transform ctx (Frame.matrix (frame t));
  Kind.render (kind t) ctx;
  Ctx.restore ctx

let touched_by t p =
  let frame =
    match (kind t) with
    | Horizontal_line
    | Vertical_line
    | Cross_line ->
      Frame.remove_scale (frame t)
    | Circle
    | Rectangle
    | Zigzag _
    | Bezier _ ->
      (frame t)
  in
  let m_inv = Matrix.inv (Frame.matrix frame) in
  let p = Matrix.apply m_inv p in
  Kind.touched_by (kind t) p

let set_alpha t ~alpha =
  set t ~color:(Color_cycle.set_alpha (color t) ~alpha)

let set_translation t v =
  set t ~frame:(Frame.set_translation (frame t) v)

let set_touches t ~touches =
  let invert vs =
    let m = Frame.matrix (frame t) |> Matrix.inv in
    List.map vs ~f:(Matrix.apply m)
  in
  let kind =
    match (kind t) with
    | ( Horizontal_line
          | Vertical_line
          | Cross_line
          | Circle
          | Rectangle) as kind -> kind
    | Zigzag _ -> Zigzag (invert touches)
    | Bezier _ -> Bezier (invert touches)
  in
  set t ~kind

let scale_to_fit t size =
  let line_width = line_width t in
  let current_size = max (width (kind t)) (height (kind t)) ~line_width in
  let s = min 1.0 (size /. current_size) in
  let frame = Frame.((frame t) *> scale ~scale_x:s ~scale_y:s) in
  set t ~frame

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
  set t ~frame:(Frame.scale_viewport (frame t) scale)
