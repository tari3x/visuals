(*
   Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
   This file is distributed under a BSD license.
   See LICENSE file for copyright notice.
*)

open Base
open Js_of_ocaml
open Js
module Html = Dom_html

module DisplayObject = struct
  type witness

  class type js = object
    method display_object_witness : witness
  end

  type t = js Js.t
end

module Matrix = struct
  type witness

  class type js = object
    method witness : witness
    method fromArray : float js_array Js.t -> unit meth

    method set :
      float -> float -> float -> float -> float -> float -> unit meth
  end

  type t = js Js.t

  let constr : t constr = Js.Unsafe.global ##. PIXI ##. Matrix
  let create () = new%js constr
end

module Container = struct
  type witness

  class type js = object
    inherit DisplayObject.js
    method container_witness : witness
    method addChild : DisplayObject.t -> unit meth
  end

  type t = js Js.t
end

module Renderer = struct
  type witness

  class type js = object
    method renderer_witness : witness
  end

  type t = js Js.t
end

module Application = struct
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

  let constr : t constr = Js.Unsafe.global ##. PIXI ##. Application
  let create () = new%js constr
end

module Color = struct
  type t = int

  let create r g b = (r * 256 * 256) + (g * 256) + b
  let white = create 255 255 255
end

module FillStyle = struct
  class type js = object
    method color : Color.t optdef readonly_prop
    method alpha : float optdef readonly_prop
  end

  type t = js Js.t

  let create ?color ?alpha () =
    object%js
      val color = Optdef.option color
      val alpha = Optdef.option alpha
    end
  ;;
end

module StrokeAttributes = struct
  class type js = object
    method width : float optdef readonly_prop
  end

  type t = js Js.t

  let create ?width () =
    object%js
      val width = Optdef.option width
    end
  ;;
end

module StrokeStyle = struct
  class type js = object
    inherit FillStyle.js
    inherit StrokeAttributes.js
  end

  type t = js Js.t

  let create ?color ?alpha ?width () =
    object%js
      val color = Optdef.option color
      val alpha = Optdef.option alpha
      val width = Optdef.option width
    end
  ;;
end

module Graphics = struct
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

  let constr : t constr = Js.Unsafe.global ##. PIXI ##. Graphics
  let create () = new%js constr
end
