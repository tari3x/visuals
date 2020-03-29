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

  class type js =
    object
      method display_object_witness : witness
    end

  type t = js Js.t
end

module Container = struct
  type witness

  class type js =
    object
      inherit DisplayObject.js

      method container_witness : witness

      method addChild : DisplayObject.t -> unit meth
    end

  type t = js Js.t
end

module Renderer = struct
  type witness

  class type js =
    object
      method renderer_witness : witness

      method resize : int -> int -> unit meth
    end

  type t = js Js.t
end

module Application = struct
  module ResizeTo = struct
    type t

    let e = Js.Unsafe.eval_string
    let window : t Js.t = e "Window"
    let html_element : t Js.t = e "HTMLElement"
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

  let constr : t constr = Js.Unsafe.global ##. PIXI ##. Application
  let create () = new%js constr
end

module Color = struct
  type t = int

  let create r g b = (r * 256 * 256) + (g * 256) + b
  let white = create 255 255 255
end

module Graphics = struct
  type witness

  module LineStyleParams = struct
    class type js =
      object
        method width : float optdef_prop

        method color : Color.t optdef_prop

        method alpha : float optdef_prop

        method alignment : float optdef_prop

        method native : bool Js.t optdef_prop
      end

    type t = js Js.t

    let create ?width ?color ?alpha ?alignment ?native () =
      let t : t = Caml.Obj.magic (object%js end) in
      Option.iter color ~f:(fun color -> t##.color := color);
      Option.iter width ~f:(fun width -> t##.width := width);
      Option.iter alpha ~f:(fun alpha -> t##.alpha := alpha);
      Option.iter alignment ~f:(fun alignment ->
          t##.alignment := alignment);
      Option.iter native ~f:(fun native -> t##.native := native);
      t
    ;;
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

      (* CR-someday going via params is broken right now:
         https://github.com/pixijs/pixi.js/issues/6490

        method lineStyle : LineStyleParams.t -> unit meth

      *)
      method lineStyle :
        width:float -> color:Color.t -> alpha:float -> unit meth

      method moveTo : float -> float -> unit meth

      method lineTo : float -> float -> unit meth

      method closePath : unit meth
    end

  type t = js Js.t

  let constr : t constr = Js.Unsafe.global ##. PIXI ##. Graphics
  let create () = new%js constr
end
