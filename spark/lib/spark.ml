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
module Config = Config.Skin
module Shape = Shapes.Shape

let max_keep_raining_probability = 1.

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
  ;;
end

module Elt = struct
  module Id = Id (struct
    let name = "Grid_elt"
  end)

  type t =
    { id : Id.t
    ; shape : Shape.t
    ; mutable color : Color_flow.t option
    }
  [@@deriving sexp, fields]

  let create ~shape = { id = Id.create (); shape; color = None }
  let centre t = Shape.centre t.shape

  (* This used to try and find the distance to the actual line rather than to
     the centre. Not sure what the point was, try the new method out. *)
  let distance_to_point t ~point =
    let open Vector in
    length (centre t - point)
  ;;

  let offset t1 t2 =
    let open Vector in
    centre t1 - centre t2
  ;;

  let touch t color = t.color <- Some color
  let color t = Option.map t.color ~f:CF.eval

  let render t ~perspective ~pixi =
    match color t with
    | None -> ()
    | Some color -> Shape.render t.shape ~perspective ~pixi ~color
  ;;
end

module Skin = Skin.Make (Elt)

type t =
  { config : Config.t
  ; pixi : Pixi.t
  ; perspective : Matrix.t
  ; skin : Skin.t
  ; mutable elts : Elt.t list
  }

let create
    ~(config : Config.t)
    ~pixi
    ~sound
    ~(shapes : Shapes.t)
    ?real_corners
    ()
  =
  let native_corners = Shapes.corners shapes in
  let real_corners = Option.value real_corners ~default:native_corners in
  let perspective =
    Prism.Surface.create ~canvas:real_corners ~camera:native_corners
    |> Prism.Surface.camera_to_canvas
  in
  let elts =
    Shapes.shapes shapes |> List.map ~f:(fun shape -> Elt.create ~shape)
  in
  (* elts are not empty by interface *)
  let skin = Skin.Elts.create_exn elts |> Skin.start ~config ~sound in
  { config; pixi; perspective; elts; skin }
;;

let ctl t box =
  match Box.kind box with
  | Ctl.Spot ->
    let color = Box.color box in
    let touches = Box.touches box ~coordinates:`canvas in
    List.filter_map touches ~f:(fun point ->
        List.min_elt t.elts ~compare:(fun s1 s2 ->
            Float.compare
              (Elt.distance_to_point ~point s1)
              (Elt.distance_to_point ~point s2)))
    |> List.dedup_and_sort ~compare:Poly.compare
    |> List.iter ~f:(fun elt -> Skin.human_touch t.skin elt color)
  | Ctl.Rain_control ->
    (match Box.touches box ~coordinates:`canvas with
    | [] -> debug [%message "box with no touches!"]
    | touch :: _ ->
      let open Float in
      let x, y = Vector.coords touch in
      let w = Pixi.width t.pixi in
      let h = Pixi.height t.pixi in
      t.config.bot_active <- x / w > 0.5;
      t.config.rain.keep_raining_probability
        <- max_keep_raining_probability * (y / h))
  | Ctl.Set_shapes shapes ->
    let elts = List.map shapes ~f:(fun shape -> Elt.create ~shape) in
    t.elts <- elts;
    (* elts are not empty by interface. *)
    Skin.Elts.create_exn elts |> Skin.set_elts t.skin
;;

let render t =
  let perspective = t.perspective in
  List.iter t.elts ~f:(Elt.render ~perspective ~pixi:t.pixi)
;;
