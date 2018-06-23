(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Js
open Common
open Geometry
open Dom_wrappers
open Prism

type t =
  { shapes : Shape.t list
  ; calibration_points : Quad.t
  } [@@deriving sexp]

module Svg_list = struct
  type 'a t = 'a Js.t Dom_svg.list Js.t

  let to_list (t : 'a t) : 'a Js.t list =
    List.init t##.numberOfItems ~f:(fun i -> t##getItem i)
end

(* This is not a correct representation. One should cast based on the value of
   [pathSegType]. But this works for now. *)
module Path_seg = struct
  type witness
  class type js = object
    inherit Dom_svg.pathSeg
    method path_seg_witness : witness
    method x : float readonly_prop
    method y : float readonly_prop
  end

  type t = js Js.t

  let of_path_seg (seg : Dom_svg.pathSeg Js.t) : t =
    Caml.Obj.magic seg

  let vector (t : t) =
    Vector.create_float t##.x t##.y
end

module Circle = struct
  type t = Dom_svg.circleElement Js.t

  let center (t : t) =
    let x = t##.cx##.baseVal##.value in
    let y = t##.cy##.baseVal##.value in
    Vector.create_float x y
end

let collect_segments (segments : Path_seg.t list) =
  match segments with
  | [] -> []
  | seg0 :: _ ->
    let v0 = Path_seg.vector seg0 in
    let rec collect v1 = function
      | [] -> []
      | (seg : Path_seg.t) :: segs ->
        let v2 = Path_seg.vector seg in
        match seg##.pathSegType with
        | PATHSEG_LINETO_ABS ->
          (v1, v2) :: collect v2 segs
        | PATHSEG_LINETO_REL ->
          let v2 = Vector.(v1 + v2) in
          (v1, v2) :: collect v2 segs
        | PATHSEG_CLOSEPATH ->
          (v1, v0) :: collect v0 segs
        | _ ->
          collect v2 segs
    in
    collect v0 segments

(* inspired by
   view-source:http://xn--dahlstrm-t4a.net/svg/html/get-embedded-svg-document-script.html

   I couldn't make the object and the embed options to work, but iframe works
   fine.

   There doesn't seem to be a way to coerce Dom_html.document to
   Dom_svg.docuemnt, but you can convert the elements once you got them.  *)
let shapes (svg_document : Dom_html.document Js.t) =
  svg_document##getElementsByTagName (string "path")
  |> Node_list.to_list
  |> List.concat_map ~f:(fun path ->
    let path =
      Dom_svg.CoerceTo.element path
      |> Opt.value_exn ~here:[%here]
      |> Dom_svg.CoerceTo.path
      |> Opt.value_exn ~here:[%here]
    in
    (* CR-someday: normalizedPathSegList blows up. *)
    path##.pathSegList
    |> Svg_list.to_list
    |> List.map ~f:Path_seg.of_path_seg
    |> collect_segments
    |> List.map ~f:(fun (v1, v2) -> Shape.segment v1 v2))

let calibration_points (svg_document : Dom_html.document Js.t) =
  svg_document##getElementsByTagName (string "circle")
  |> Node_list.to_list
  |> List.map ~f:(fun circle ->
      Dom_svg.CoerceTo.element circle
      |> Opt.value_exn ~here:[%here]
      |> Dom_svg.CoerceTo.circle
      |> Opt.value_exn ~here:[%here])
  |> function
    | [c1; c2; c3; c4] ->
      let c = Circle.center in
      Quad.create (c c1) (c c2) (c c3) (c c4)
    | _ -> failwith "expecting exactly 4 circles"

let parse_exn (elt : Html.iFrameElement Js.t) =
  let doc = Opt.value_exn elt##.contentDocument ~here:[%here] in
  let shapes = shapes doc in
  let calibration_points = calibration_points doc in
  let t = { shapes; calibration_points } in
  debug !"%{sexp:t}" t;
  t
