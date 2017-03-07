(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Geometry
open Dom_wrappers

val draw_markers : ?m:Matrix.t -> Ctx.t -> unit

(* Returns the transformation from canvas coordinates to camera image
   coordinates. Assumes that mouse click coordinates are the same as canvas
   coordinates. *)
val canvas_to_camera : Ctx.t -> Matrix.t Lwt.t
