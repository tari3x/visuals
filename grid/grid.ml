(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Std_internal

(* TODO:
   - defining segment groups that flash together.
   - unify the straight lines in the picture. Not sure which way is better.
   - two independent projectors
*)

let flash_cutoff = 0.6 (* 0.7 *)
let line_width = 8. (* 18. *)
let shorten_by = 0. (* 7. *)
let max_keep_raining_probability = 1.
let segment_life_span =
  if Config.drawing_mode
  then Float.infty
  else 3.

let human_playing_timeout = Time.Span.of_sec 10.

module Ctl = struct
  type t =
  | Spot
  | Rain_control
  | Set_segments of (Vector.t * Vector.t) list
      [@@deriving sexp]
end

module Segment = struct
  module Kind = struct
    type t = [ `vertical | `horizontal | `free ] [@@deriving sexp]
  end

  type t =
    { kind : Kind.t
    ; v1 : Vector.t
    ; v2 : Vector.t
    ; mutable last_touched : Time.t option
    ; mutable color : Color.t
    ; mutable flash : bool
    } [@@deriving sexp]

  let create v1 v2 ~kind ~color =
    { kind
    ; v1
    ; v2
    ; last_touched = Some (Time.now ())
    ; color
    ; flash = false
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
    | `free -> infty

  let touch t ~color ~flash =
    let color =
      Color.interpolate [t.color; color] ~arg:0.5
    in
    t.color <- color;
    t.flash <- flash;
    t.last_touched <- Some (Time.now ())

  let shortened_ends t =
    let open Vector in
    let delta = (normalize (t.v2 - t.v1)) * shorten_by in
    t.v1 + delta, t.v2 - delta

  let render t ~perspective ~ctx ~sound:_ =
    let open Float in
    match t.last_touched with
    | None -> ()
    | Some t0 ->
      let delta = Time.(now () - t0) |> Time.Span.to_sec in
      let alpha = 1. - (delta / segment_life_span) in
      let alpha =
        if delta < 0.1 && t.flash
        then 1.
        else alpha * flash_cutoff
      in
      (* let alpha = alpha *. Sound.volume sound in *)
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
  ; sound : Sound.t
  ; top_left : Vector.t
  ; width : float
  ; height : float
  ; perspective : Matrix.t
  ; color : Color.t
  ; mutable segments : Segment.t list
  ; mutable last_human_touch : Time.t
  ; mutable bot_active : bool
  ; mutable keep_raining_probability : float
  }

let rec rain t ~base_color ~is_first =
  let open Float in
  let segment = List.random_element_exn t.segments in
  let color = base_color in
  (* interpolation in touch should already create enough variation. *)
  (*
  let color =
    Color.interpolate [ base_color; Color.random () ] ~arg:0.1
  in
  *)
  Segment.touch segment ~color ~flash:is_first;
  if Random.float 1. > t.keep_raining_probability
  then Lwt.return ()
  else begin
    Lwt_js.sleep 0.05
    >>= fun () ->
    rain t ~base_color ~is_first:false
  end

let human_playing t =
  let open Time in
  let open Span in
  now () - t.last_human_touch < human_playing_timeout

let start_rain ?base_color t =
  let base_color =
    match base_color with
    | Some base_color -> base_color
    | None -> Color.random () |> Color.maximize
  in
  Lwt.async (fun () -> rain t ~base_color ~is_first:true)

let grid_segments ~top_left ~width ~height ~color ~rows ~cols =
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
    let kind = (kind :> Segment.Kind.t) in
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

module Segments = struct
  type t =
  | Grid of { rows: int; cols : int }
  | Set of (Vector.t * Vector.t) list
end

let create ~ctx ~sound
    ~(segments : Segments.t)
    ?native_corners ?real_corners ~color () =
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
  let segments =
    match segments with
    | Set segments ->
      List.map segments ~f:(fun (v1, v2) ->
        Segment.create v1 v2 ~kind:`free ~color)
    | Grid { rows; cols } ->
      grid_segments ~top_left ~width ~height ~color ~rows ~cols
  in
  let t =
    { ctx
    ; sound
    ; top_left
    ; width
    ; height
    ; perspective
    ; color
    ; segments
    ; last_human_touch = Time.(sub (now ()) human_playing_timeout)
    ; bot_active = Config.bot_active_at_start
    ; keep_raining_probability = 0.9
    }
  in
  Sound.on_beat sound ~f:(fun source ->
    if not (human_playing t) && t.bot_active
    then start_rain t ~base_color:(Sound.Source.color source)
  );
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
      |> List.iter ~f:(Segment.touch ~color ~flash:true)
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
        t.bot_active <- (x / w) > 0.5;
        t.keep_raining_probability <-
          max_keep_raining_probability * (y / h)
    end
  | Ctl.Set_segments segments ->
    t.segments <-
      List.map segments ~f:(fun (v1, v2) ->
        Segment.create v1 v2 ~kind:`free ~color:t.color)

let render t =
  let perspective = t.perspective in
  Ctx.clear t.ctx;
  List.iter t.segments ~f:(Segment.render ~perspective ~ctx:t.ctx ~sound:t.sound)
