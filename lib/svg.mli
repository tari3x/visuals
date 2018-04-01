open Common
open Geometry
open Prism

type t =
  { segments : (Vector.t * Vector.t) list
  ; calibration_points : Quad.t
  }

val parse_exn : Html.iFrameElement Js.t -> t
