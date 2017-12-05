(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Util
open Common
open Geometry
open Dom_wrappers
open Remote

let line_width = 18.
let shorten_by = 7.
let max_start_raining_probability = 0.2
let max_keep_raining_probability = 1.
let segment_life_span =
  if Config.drawing_mode
  then Float.infty
  else 3.

module Ctl = struct
  type t =
  | Spot
  | Rain_control
      [@@deriving sexp]
end

module Segment = struct
  type t =
    { kind : [ `vertical | `horizontal ]
    ; v1 : Vector.t
    ; v2 : Vector.t
    ; mutable last_touched : Time.t option
    ; mutable color : Color.t
    } [@@deriving sexp]

  let create v1 v2 ~kind ~color =
    { kind
    ; v1
    ; v2
    ; last_touched = Some (Time.now ())
    ; color
    }

  let distance t ~point =
    (*
      debug !"segment: %{sexp:t}" t;
      debug !"point: %{sexp:Vector.t}" point;
    *)
    let open Float in
    let infty = 1_000_000_000. in
    let (x, y)   = Vector.coords point in
    let (x1, y1) = Vector.coords t.v1 in
    let (x2, y2) = Vector.coords t.v2 in
    let rec between a a1 a2 =
      if a1 < a2
      then a1 < a && a < a2
      else between a a2 a1
    in
    match t.kind with
    | `vertical ->
      if between y y1 y2 then abs (x - x1) else infty
    | `horizontal ->
      if between x x1 x2 then abs (y - y1) else infty

  let touch t ~color =
    let color =
      Color.interpolate [t.color; color] 0.5
    in
    t.color <- color;
    t.last_touched <- Some (Time.now ())

  let shortened_ends t =
    let open Vector in
    let delta = (normalize (t.v2 - t.v1)) * shorten_by in
    t.v1 + delta, t.v2 - delta

  let render t ~perspective ~ctx =
    let open Float in
    match t.last_touched with
    | None -> ()
    | Some t0 ->
      let delta = Time.(now () - t0) |> Time.Span.to_seconds in
      let alpha = 1. - (delta / segment_life_span) in
      if alpha < 0.
      then t.last_touched <- None
      else begin
        let color = Color.set_alpha t.color ~alpha in
        Ctx.set_stroke_color ctx color;
        Ctx.set_line_width ctx line_width;
        let v1, v2 = shortened_ends t in
        let v1 = Matrix.apply perspective v1 in
        let v2 = Matrix.apply perspective v2 in
        Ctx.begin_path ctx;
        Ctx.move_to ctx v1;
        Ctx.line_to ctx v2;
        Ctx.stroke ctx
      end
end

type t =
  { ctx : Ctx.t
  ; rows : int
  ; cols : int
  ; top_left : Vector.t
  ; width : float
  ; height : float
  ; perspective : Matrix.t
  ; segments : Segment.t list
  ; mutable last_human_touch : Time.t
  ; mutable start_raining_probability : float
  ; mutable keep_raining_probability : float
  }

let rec rain t ~base_color =
  let open Float in
  let segment = List.random_element_exn t.segments in
  let color =
    Color.interpolate [ base_color; Color.random () ] 0.1
  in
  Segment.touch segment ~color;
  if Random.float 1. > t.keep_raining_probability
  then Lwt.return ()
  else begin
    Lwt_js.sleep 0.05
    >>= fun () ->
    rain t ~base_color
  end

let rec rain_loop t =
  let open Float in
  begin
    if Time.(now () - t.last_human_touch |> Span.to_seconds) > 10.
      (* CR-someday: 0.1? *)
      && Random.float 0.1 < t.start_raining_probability
    then begin
      let base_color = Color.random () |> Color.maximize in
      Lwt.async (fun () -> rain t ~base_color)
    end
  end;
  Lwt_js.sleep 1.
  >>= fun () ->
  rain_loop t

(* camera in this context means "native" or source coordinates. *)
let create ~ctx ~rows ~cols ?corners ~color () =
  let wmargin = Ctx.width ctx *. 0.1 in
  let hmargin = Ctx.height ctx *. 0.1 in
  let width = Ctx.width ctx -. 2. *. wmargin in
  let height = Ctx.height ctx -. 2. *. hmargin in
  let top_left = Vector.create_float wmargin hmargin in
  let camera =
    let w = width in
    let h = height in
    let tr dx dy = Vector.(top_left + Vector.create_float dx dy) in
    let v1 = tr 0. 0. in
    let v2 = tr w  0. in
    let v3 = tr w  h  in
    let v4 = tr 0. h  in
    Prism.Quad.create v1 v2 v3 v4
  in
  let canvas = Option.value corners ~default:camera in
  let perspective =
    Prism.Surface.create ~canvas ~camera
    |> Prism.Surface.camera_to_canvas
  in
  let segments =
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
      else Some (Segment.create (point row col) (point row' col') ~kind ~color)
    in
    let make ~kind =
      List.init (rows + 1) ~f:(fun row ->
        List.init (cols + 1) ~f:(fun col ->
          segment ~row ~col ~kind))
      |> List.concat
      |> List.filter_opt
    in
    make ~kind:`horizontal @ make ~kind:`vertical
  in
  let t =
    { rows
    ; cols
    ; ctx
    ; top_left
    ; width
    ; height
    ; perspective
    ; segments
    ; last_human_touch = Time.now ()
    ; start_raining_probability = 0.
    ; keep_raining_probability  = 0.
    }
  in
  Lwt.async (fun () -> rain_loop t);
  t

let ctl t box =
  match Box.kind box with
  | Ctl.Spot ->
    begin
      t.last_human_touch <- Time.now ();
      let color = Box.color box in
      let touches = Box.touches box ~coordinates:`canvas in
      List.filter_map touches ~f:(fun point ->
        List.min_elt t.segments ~cmp:(fun s1 s2 ->
          Float.compare
            (Segment.distance ~point s1)
            (Segment.distance ~point s2)))
      |> List.dedup_and_sort
      |> List.iter ~f:(Segment.touch ~color)
    end
  | Ctl.Rain_control ->
    match Box.touches box ~coordinates:`canvas with
    | [] ->
      debug "box with no touches!"
    | touch :: _ ->
      let open Float in
      let (x, y) = Vector.coords touch in
      let w = Ctx.width t.ctx in
      let h = Ctx.height t.ctx in
      t.start_raining_probability <-
        max_start_raining_probability * (x / w);
      t.keep_raining_probability <-
        max_keep_raining_probability * (y / h)

let render t =
  let perspective = t.perspective in
  Ctx.clear t.ctx;
  List.iter t.segments ~f:(Segment.render ~perspective ~ctx:t.ctx)
