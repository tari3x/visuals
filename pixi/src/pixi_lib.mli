(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

(* CR-someday: there's also two.js. *)

open Js_of_ocaml
open Raw

module DisplayObject : sig
  include module type of DisplayObject
end

module Container : sig
  include module type of Container

  val add_child : t -> DisplayObject.t -> unit
end

module Renderer : sig
  include module type of Renderer

  val resize : t -> int -> int -> unit
end

module Application : sig
  include module type of Application

  module ResizeTo : sig
    type t =
      | Window
      | Html_element
  end

  val create : unit -> t
  val view : t -> Html.canvasElement Js.t
  val stage : t -> Container.t

  (* CR-someday: what does this do?
     https://github.com/pixijs/pixi.js/issues/6492
  *)
  val resize : t -> ResizeTo.t -> unit
  val renderer : t -> Renderer.t
end

module Color : sig
  type t = Color.t

  val create : int -> int -> int -> t
end

(* CR-someday: polygon *)
(* CR-someday: matrix transformations. *)
module Graphics : sig
  include module type of Graphics

  val create : unit -> t
  val clear : t -> unit
  val width : t -> float
  val height : t -> float
  val draw_circle : t -> x:float -> y:float -> radius:float -> unit
  val begin_fill : t -> color:Color.t -> ?alpha:float -> unit -> unit
  val end_fill : t -> unit
  val move_to : t -> float -> float -> unit
  val line_to : t -> float -> float -> unit
  val close_path : t -> unit

  val line_style
    :  t
    -> ?width:float
    -> ?color:Color.t
    -> ?alpha:
         float (*
    -> ?alignment:float
    -> ?native:bool Js.t
    *)
    -> unit
    -> unit
end
