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

  class type js =
    object
      method display_object_witness : witness
    end

  type t = js Js.t
end

module Container : sig
  type witness

  class type js =
    object
      inherit DisplayObject.js

      method container_witness : witness

      method addChild : DisplayObject.t -> unit meth
    end

  type t = js Js.t
end

module Renderer : sig
  type witness

  class type js =
    object
      method renderer_witness : witness

      method resize : int -> int -> unit meth
    end

  type t = js Js.t
end

module Application : sig
  module ResizeTo : sig
    type t

    val window : t Js.t
    val html_element : t Js.t
  end

  type witness

  class type js =
    object
      method application_witness : witness

      method view : Html.canvasElement Js.t readonly_prop

      method stage : Container.t readonly_prop

      method resizeTo : ResizeTo.t Js.t prop

      method resize : unit meth

      method renderer : Renderer.t readonly_prop
    end

  type t = js Js.t

  val create : unit -> t
end

module Color : sig
  type t

  val create : int -> int -> int -> t
  val white : t
end

module Graphics : sig
  type witness

  module LineStyleParams : sig
    type t

    val create
      :  ?width:float
      -> ?color:Color.t
      -> ?alpha:float
      -> ?alignment:float
      -> ?native:bool Js.t
      -> unit
      -> t
  end

  class type js =
    object
      inherit Container.js

      method graphics_witness : witness

      method clear : unit meth

      method width : float readonly_prop

      method height : float readonly_prop

      method drawCircle : x:float -> y:float -> radius:float -> unit meth

      method beginFill : color:Color.t -> alpha:float -> unit meth

      method endFill : unit meth

      method moveTo : float -> float -> unit meth

      method lineTo : float -> float -> unit meth

      method closePath : unit meth

      method lineStyle :
        width:float -> color:Color.t -> alpha:float -> unit meth
    end

  type t = js Js.t

  val create : unit -> t
end
