(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

(* TODO:
   - defining segment groups that flash together.
   - unify the straight lines in the picture. Not sure which way is better.
   - two independent projectors
*)

module CF = Color_flow

let line_width = 8. (* 18. *)
let shorten_by = 0. (* 7. *)
let max_keep_raining_probability = 1.

module Shape = struct
  include Shape

  let centre t =
    let open Vector in
    match t with
    | Segment (v1, v2) ->
      (v1 + v2) / 2.
    | Polygon vs ->
      let n = List.length vs in
      List.reduce_exn vs ~f:(+) / float n

  let shortened_ends v1 v2 =
    let open Vector in
    let delta = (normalize (v2 - v1)) * shorten_by in
    v1 + delta, v2 - delta

  let render t ~perspective ~ctx ~color =
    match t with
    | Segment (v1, v2) ->
      Ctx.set_stroke_color ctx color;
      Ctx.set_line_width ctx line_width;
      let v1, v2 = shortened_ends v1 v2 in
      let v1 = Matrix.apply perspective v1 in
      let v2 = Matrix.apply perspective v2 in
      Ctx.path ctx ~closed:false [v1; v2];
      Ctx.stroke ctx
    | Polygon vs ->
      Ctx.set_fill_color ctx color;
      let vs = List.map vs ~f:(Matrix.apply perspective) in
      Ctx.path ctx ~closed:true vs;
      Ctx.fill ctx
end

module Ctl = struct
  type t =
  | Spot
  | Rain_control
  | Set_shapes of Shape.t list
      [@@deriving sexp]

  let spot = Spot
  let rain_control = Rain_control

  let set_shapes_exn shapes =
    if List.is_empty shapes then failwith "Ctl.Set_shapes: empty list";
    Set_shapes shapes
end

module Elt = struct
  module Id = Id(struct let name = "Grid_elt" end)

  type t =
    { id : Id.t
    ; shape : Shape.t
    ; mutable color : Color_flow.t option
    } [@@deriving sexp, fields]

  let create ~shape =
    { id = Id.create ()
    ; shape
    ; color = None
    }

  let centre t =
    Shape.centre t.shape

  (* This used to try and find the distance to the actual line rather than to
     the centre. Not sure what the point was, try the new method out. *)
  let distance_to_point t ~point =
    let open Vector in
    length (centre t - point)

  let distance t1 t2 =
    let open Vector in
    length (centre t1 - centre t2)

  let touch t color =
    t.color <- Some color

  let render t ~perspective ~ctx =
    match t.color with
    | None -> ()
    | Some color ->
      let color = CF.eval color in
      Shape.render t.shape ~perspective ~ctx ~color
end

module Skin = Skin.Make(Elt)

type t =
  { config : Config.t
  ; ctx : Ctx.t
  ; top_left : Vector.t
  ; width : float
  ; height : float
  ; perspective : Matrix.t
  ; skin : Skin.t
  ; mutable elts : Elt.t list
  }

let grid_segments ~top_left ~width ~height ~rows ~cols =
  let dy = height /. (float rows) in
  let dx = width  /. (float cols) in
  let point row col =
    Vector.(top_left + create_float ((float col) *. dx) ((float row) *. dy))
  in
  let segment ~row ~col ~kind =
    let row', col' =
      match kind with
      | `vertical   -> row + 1, col
      | `horizontal -> row, col + 1
    in
    if row' > rows || col' > cols
    then None
    else begin
      let shape = Shape.segment (point row col) (point row' col') in
      Some (Elt.create ~shape)
    end
  in
  let make ~kind =
    List.init (rows + 1) ~f:(fun row ->
      List.init (cols + 1) ~f:(fun col ->
        segment ~row ~col ~kind))
    |> List.concat
    |> List.filter_opt
  in
  make ~kind:`horizontal @ make ~kind:`vertical

module Shapes = struct
  type t =
  | Grid of { rows: int; cols : int }
  | Set of Shape.t list

  let grid_exn ~rows ~cols =
    if rows <= 0 || cols <= 0 then failwith "rows or cols not positive";
    Grid { rows; cols }

  let set_exn shapes =
    if List.is_empty shapes then failwith "Shapes.set_exn: empty list";
    Set shapes
end

let create ~(config : Config.t) ~ctx ~sound ~(shapes : Shapes.t)
    ?native_corners ?real_corners () =
  let wmargin = Ctx.width ctx *. 0.1 in
  let hmargin = Ctx.height ctx *. 0.1 in
  let width = Ctx.width ctx -. 2. *. wmargin in
  let height = Ctx.height ctx -. 2. *. hmargin in
  let top_left = Vector.create_float wmargin hmargin in
  let native_corners =
    match native_corners with
    | Some corners -> corners
    | None ->
      let w = width in
      let h = height in
      let tr dx dy = Vector.(top_left + Vector.create_float dx dy) in
      let v1 = tr 0. 0. in
      let v2 = tr w  0. in
      let v3 = tr w  h  in
      let v4 = tr 0. h  in
      Prism.Quad.create v1 v2 v3 v4
  in
  let real_corners = Option.value real_corners ~default:native_corners in
  let perspective =
    Prism.Surface.create ~canvas:real_corners ~camera:native_corners
    |> Prism.Surface.camera_to_canvas
  in
  let elts =
    match shapes with
    | Set shapes ->
      List.map shapes ~f:(fun shape -> Elt.create ~shape)
    | Grid { rows; cols } ->
      grid_segments ~top_left ~width ~height ~rows ~cols
  in
  (* elts are not empty by interface *)
  let skin =
    Skin.Elts.create_exn elts
    |> Skin.start ~config ~sound
  in
  { config
  ; ctx
  ; top_left
  ; width
  ; height
  ; perspective
  ; elts
  ; skin
  }

let ctl t box =
  match Box.kind box with
  | Ctl.Spot ->
    begin
      let color = Box.color box in
      let touches = Box.touches box ~coordinates:`canvas in
      List.filter_map touches ~f:(fun point ->
        List.min_elt t.elts ~compare:(fun s1 s2 ->
          Float.compare
            (Elt.distance_to_point ~point s1)
            (Elt.distance_to_point ~point s2)))
      |> List.dedup_and_sort ~compare:Poly.compare
      |> List.iter ~f:(fun elt -> Skin.human_touch t.skin elt color)
    end
  | Ctl.Rain_control ->
    begin
      match Box.touches box ~coordinates:`canvas with
      | [] ->
        debug "box with no touches!"
      | touch :: _ ->
        let open Float in
        let (x, y) = Vector.coords touch in
        let w = Ctx.width t.ctx in
        let h = Ctx.height t.ctx in
        t.config.bot_active <- (x / w) > 0.5;
        t.config.keep_raining_probability <-
          max_keep_raining_probability * (y / h)
    end
  | Ctl.Set_shapes shapes ->
    let elts = List.map shapes ~f:(fun shape -> Elt.create ~shape) in
    t.elts <- elts;
    (* elts are not empty by interface. *)
    Skin.Elts.create_exn elts
    |> Skin.set_elts t.skin

let render t =
  let perspective = t.perspective in
  Ctx.clear t.ctx;
  List.iter t.elts ~f:(Elt.render ~perspective ~ctx:t.ctx)
