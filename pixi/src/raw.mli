(*
   Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
   This file is distributed under a BSD license.
   See LICENSE file for copyright notice.
*)

open Js_of_ocaml
open Js
module Html = Dom_html

module DisplayObject : sig
  type witness

  class type js = object
    method display_object_witness : witness
  end

  type t = js Js.t
end

module Matrix : sig
  type witness

  class type js = object
    method witness : witness
    method fromArray : float js_array Js.t -> unit meth

    method set :
      float -> float -> float -> float -> float -> float -> unit meth
  end

  type t = js Js.t

  val create : unit -> t
end

module Container : sig
  type witness

  class type js = object
    inherit DisplayObject.js
    method container_witness : witness
    method addChild : DisplayObject.t -> unit meth
  end

  type t = js Js.t
end

module Renderer : sig
  type witness

  class type js = object
    method renderer_witness : witness
  end

  type t = js Js.t
end

module Application : sig
  type witness

  class type js = object
    method application_witness : witness
    method view : Html.canvasElement Js.t readonly_prop
    method stage : Container.t readonly_prop
    method resize : unit meth
    method resizeTo : Html.window Js.t prop
    method renderer : Renderer.t readonly_prop
    method init : unit Promise.t meth
  end

  type t = js Js.t

  val create : unit -> t
end

module Color : sig
  type t

  val create : int -> int -> int -> t
  val white : t
end

module FillStyle : sig
  class type js = object
    method color : Color.t optdef readonly_prop
    method alpha : float optdef readonly_prop
  end

  type t = js Js.t

  val create : ?color:Color.t -> ?alpha:float -> unit -> t
end

module StrokeAttributes : sig
  class type js = object
    method width : float optdef readonly_prop
  end

  type t = js Js.t

  val create : ?width:float -> unit -> t
end

module StrokeStyle : sig
  class type js = object
    inherit FillStyle.js
    inherit StrokeAttributes.js
  end

  type t = js Js.t

  val create : ?color:Color.t -> ?alpha:float -> ?width:float -> unit -> t
end

module Graphics : sig
  type witness

  class type js = object
    inherit Container.js
    method graphics_witness : witness
    method clear : unit meth
    method width : float readonly_prop
    method height : float readonly_prop
    method drawCircle : x:float -> y:float -> radius:float -> unit meth

    method drawRect :
      x:float -> y:float -> width:float -> height:float -> unit meth

    method fill : FillStyle.t -> unit meth
    method stroke : StrokeStyle.t -> unit meth
    method moveTo : float -> float -> unit meth
    method lineTo : float -> float -> unit meth
    method closePath : unit meth
    method resetTransform : unit meth
    method transform : Matrix.t -> unit meth
  end

  type t = js Js.t

  val create : unit -> t
end
