(*
   Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
   This file is distributed under a BSD license.
   See LICENSE file for copyright notice.
*)

(* CR-someday: there's also two.js. *)

(* CR-someday: drop this extra layer, you really want your own geometry. *)

open Js_of_ocaml
open Raw

module DisplayObject : sig
  include module type of DisplayObject
end

module Container : sig
  include module type of Container

  val add_child : t -> DisplayObject.t -> unit
  val set_tint : t -> Color.t -> unit
  val set_alpha : t -> float -> unit
  val set_visible : t -> bool Js.t -> unit
  val set_zindex : t -> int -> unit
end

module Application : sig
  include module type of Application

  (* CR-someday avatar: why are create and init separate? *)
  val create : unit -> t
  val init : t -> unit Promise.t
  val view : t -> Html.canvasElement Js.t
  val stage : t -> Container.t
  val resize_to : t -> Html.window Js.t -> unit
  val resize : t -> unit
  val renderer : t -> Renderer.t
end

module Color : sig
  type t = Color.t

  val create : int -> int -> int -> t
end

(* CR-someday avatar: we really want to construct it from [Geometry.Matrix]
   which is not defined here. *)
module Matrix : sig
  include module type of Matrix

  val from_array : t -> float array -> unit
end

(* CR-someday: polygon *)
module Graphics : sig
  include module type of Graphics

  val create : unit -> t
  val clear : t -> unit
  val width : t -> float
  val height : t -> float
  val draw_circle : t -> x:float -> y:float -> radius:float -> unit

  val draw_rect
    :  t
    -> x:float
    -> y:float
    -> width:float
    -> height:float
    -> unit

  val fill : t -> color:Color.t -> ?alpha:float -> unit -> unit

  val stroke
    :  t
    -> color:Color.t
    -> ?alpha:float
    -> ?width:float
    -> unit
    -> unit

  val move_to : t -> float -> float -> unit
  val line_to : t -> float -> float -> unit
  val close_path : t -> unit
  val reset_transform : t -> unit
  val transform : t -> Matrix.t -> unit
end
