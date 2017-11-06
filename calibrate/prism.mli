(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Geometry
open Dom_wrappers

(* A collection of surfaces to project onto, each defined by 4 points.
*)

module Quad : sig
  type t

  val create : Vector.t -> Vector.t -> Vector.t -> Vector.t -> t

  val draw_border : t -> ctx:Ctx.t -> color:Color.t -> unit

  val contains : t -> Vector.t -> bool
end

module Surface : sig
  type t

  val create : canvas:Quad.t -> camera:Quad.t -> t
end

type t

val create : Surface.t list -> t

val camera_vector_to_canvas : t -> Vector.t -> Vector.t option

(* CR: change name, you aren't actually drawing. Make it just return the
   object. *)
val draw_camera_image_on_canvas
  :  t
  -> texture:Three.Texture.t
  -> scene:Three.Scene.t
  -> canvas:Canvas.t
  -> unit

val draw_border : t -> ctx:Ctx.t -> unit
