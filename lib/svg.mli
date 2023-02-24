(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Js_of_ocaml
open Common
open Geometry
open Prism

type t =
  { shapes : Shape.t list
  ; calibration_points : Quad.t
  ; step : float
  }

val parse_exn : Html.iFrameElement Js.t -> t
