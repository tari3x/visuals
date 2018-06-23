(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common
open Geometry
open Prism

type t =
  { shapes : Shape.t list
  ; calibration_points : Quad.t
  }

val parse_exn : Html.iFrameElement Js.t -> t
