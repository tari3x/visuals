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
   - remote control
*)

module CF = Color_flow
module PD = Probability_distribution

let flash_cutoff = 0.4 (* 0.7 *)
let line_width = 8. (* 18. *)
let shorten_by = 0. (* 7. *)
let max_keep_raining_probability = 1.

let human_playing_timeout = Time.Span.of_sec 10.

module Ctl = struct
  type t =
  | Spot
  | Rain_control
  | Set_segments of (Vector.t * Vector.t) list
      [@@deriving sexp]
end

  (*
module Shape = struct
  type t =
  | Segment of (Vector.t * Vector.t)
  | Polygon of Vector.t list
      [@@deriving sexp]
end
  *)

module Segment = struct
  type t =
    { config : Config.t sexp_opaque
    ; v1 : Vector.t
    ; v2 : Vector.t
    ; mutable base_color : Color.t
    ; mutable color : Color_flow.t
    } [@@deriving sexp]

  let fade ~config ~base_color ~flash =
    let open Time.Span in
    let base_color alpha = Color.set_alpha base_color ~alpha in
    let sls = Config.segment_life_span config in
    let final_alpha =
      match config.color_flow with
      | `fade_to_black -> 0.
      | `fade_to_base -> flash_cutoff
    in
    if not flash
    then begin
      CF.start_now (base_color flash_cutoff)
      |> CF.add ~after:sls ~color:(base_color final_alpha)
    end
    else begin
      CF.start_now (base_color 1.)
      |> CF.add ~after:(sls * 0.1) ~color:(base_color flash_cutoff)
      |> CF.add ~after:(sls * 0.9) ~color:(base_color final_alpha)
    end

  let touch t ~color ~flash =
    (* CR-someday: if we are faded out, why interpolate? *)
    let base_color =
      match t.config.color_flow with
      | `fade_to_black ->
        Color.interpolate [t.base_color; color] ~arg:0.5
      | `fade_to_base ->
        Color.interpolate [t.base_color; color] ~arg:0.3
    in
    t.base_color <- base_color;
    t.color <- fade ~config:t.config ~base_color ~flash

  let create v1 v2 ~config ~base_color =
    let color = fade ~config ~base_color ~flash:false in
    { config
    ; v1
    ; v2
    ; base_color
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
      if a1 <= a2
      then a1 < a && a < a2
      else between a a2 a1
    in
    let vertical =
      if between y y1 y2 then abs (x - x1) else infty
    in
    let horizontal =
      if between x x1 x2 then abs (y - y1) else infty
    in
    Float.min vertical horizontal

  let centre t =
    let open Vector in
    (t.v1 + t.v2) / 2.

  let shortened_ends t =
    let open Vector in
    let delta = (normalize (t.v2 - t.v1)) * shorten_by in
    t.v1 + delta, t.v2 - delta

  let render t ~perspective ~ctx ~sound:_ =
    let color = CF.eval t.color in
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

module Source = struct
  include Sound.Source

  module Data = struct
    type source = t
    type t =
      { source : source sexp_opaque
      ; color : Color.t
      ; segments : Segment.t PD.t sexp_opaque
      } [@@deriving sexp, fields]

    let create ~source ~segments =
      let open Float in
      let color = Color.random () |> Color.maximize in
      let centre_segment = List.random_element_exn segments in
      let c = Segment.centre centre_segment in
      let segments =
        let max_weight, other_segments =
          List.filter segments ~f:(fun s -> not (phys_equal s centre_segment))
          |> List.fold_map ~init:0. ~f:(fun max_weight segment ->
            let distance = Vector.(length (Segment.centre segment - c)) in
            let weight = int_pow (1. / distance) 3 in
            Float.max max_weight weight, (segment, weight))
        in
        (*
        let other_segments =
          List.map other_segments ~f:(fun (segment, weight) ->
            let weight =
              if weight < max_weight / 2. then 0. else weight
            in
            (segment, weight))
        in
        *)
        PD.create_exn
          (( centre_segment, max_weight ) :: other_segments)
      in
      (* debug !"segments: %{sexp: Segment.t PD.t}" segments; *)
      { source; color; segments }
  end
end

type t =
  { config : Config.t
  ; ctx : Ctx.t
  ; sound : Sound.t
  (* CR: use weak table and remove cleanup code. *)
  ; sources : Source.Data.t Hashtbl.M(Source.Id).t
  ; top_left : Vector.t
  ; width : float
  ; height : float
  ; perspective : Matrix.t
  ; base_color : Color.t
  ; mutable segments : Segment.t list
  ; mutable last_human_touch : Time.t
  ; mutable bot_active : bool
  ; mutable keep_raining_probability : float
  }

let start_cleanup_sources_loop t =
  let rec loop () =
    Lwt_js.sleep 0.01
    >>= fun () ->
    Hashtbl.filter_inplace t.sources ~f:(fun data ->
      Source.is_alive data.source);
    loop ()
  in
  loop ()

let source_data t source =
  let id = Source.id source in
  Hashtbl.find_or_add t.sources id ~default:(fun () ->
    Source.Data.create ~source ~segments:t.segments)

let rec rain t ~(source : Source.Data.t) ~is_first =
  let open Float in
  let segment = PD.draw (Source.Data.segments source) in
  let color = (Source.Data.color source) in
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
    rain t ~source ~is_first:false
  end

let start_rain t ~source =
  Lwt.async (fun () -> rain t ~source ~is_first:true)

let human_playing t =
  let open Time in
  let open Span in
  now () - t.last_human_touch < human_playing_timeout

let grid_segments ~config ~top_left ~width ~height ~base_color ~rows ~cols =
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
    else Some (Segment.create (point row col) (point row' col')
                 ~config ~base_color)
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

let create ~(config : Config.t) ~ctx ~sound
    ~(segments : Segments.t)
    ?native_corners ?real_corners ~base_color () =
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
        Segment.create v1 v2 ~config ~base_color)
    | Grid { rows; cols } ->
      grid_segments ~config ~top_left ~width ~height ~base_color ~rows ~cols
  in
  let t =
    { config
    ; ctx
    ; sound
    ; sources = Hashtbl.create (module Source.Id) ()
    ; top_left
    ; width
    ; height
    ; perspective
    ; base_color
    ; segments
    ; last_human_touch = Time.(sub (now ()) human_playing_timeout)
    ; bot_active = config.bot_active_at_start
    ; keep_raining_probability = 0.95
    }
  in
  Lwt.async (fun () -> start_cleanup_sources_loop t);
  Sound.on_beat sound ~f:(fun source ->
    if not (human_playing t) && t.bot_active
    then begin
      let source = source_data t source in
      start_rain t ~source
    end
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
        Segment.create ~config:t.config v1 v2 ~base_color:t.base_color)

let render t =
  let perspective = t.perspective in
  Ctx.clear t.ctx;
  List.iter t.segments
    ~f:(Segment.render ~perspective ~ctx:t.ctx ~sound:t.sound)
