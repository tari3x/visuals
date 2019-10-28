(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Js_of_ocaml
open Js

module Html = Dom_html

module Vector2 : sig
  type t

  val create : float -> float -> t
end

module Vector3 : sig
  type t

  val create : float -> float -> float -> t
end

module Matrix4 : sig
  type witness
  class type js = object
    method matrix4_witness : witness
    method set
      : float -> float -> float -> float
        -> float -> float -> float -> float
          -> float -> float -> float -> float
            -> float -> float -> float -> float
              -> unit meth
    method identity : unit meth
    method elements : float js_array Js.t readonly_prop
  end

  type t = js Js.t

  val create : unit -> t
end

module Color : sig
  type t

  (* rgb between 0 and 1 *)
  val create : float -> float -> float -> t
end

module Texture : sig
  module MinFilter : sig
    type t
    val nearest                : t Js.t
    val nearest_mipmap_nearest : t Js.t
    val nearest_mipmap_linear  : t Js.t
    val linear                 : t Js.t
    val linear_mipmap_nearest  : t Js.t
    val linear_mipmap_linear   : t Js.t
  end

  type witness
  class type js = object
    method texture_witness : witness
    method minFilter : MinFilter.t Js.t prop
  end

  type t = js Js.t
end

(* Loading GIF textures directly doesn't work. Use [VideoTexture]. *)
module TextureLoader : sig
  type witness
  class type js = object
    method texture_loader_witness : witness
    method load : js_string Js.t -> (Texture.t -> unit) callback -> unit meth
  end

  type t = js Js.t

  val create : unit -> t
end

module VideoTexture : sig
  type witness
  class type js = object
    inherit Texture.js
  end

  type t = js Js.t

  val create : Html.videoElement Js.t -> t
end

module Face3 : sig
  type witness
  class type js = object
    method face3_witness : witness
  end

  type t = js Js.t

  val create
    :  int -> int -> int (* indices of the vertices *)
    -> normal:Vector3.t
    -> Color.t
    -> material_index:int
    -> t
end

module Geometry : sig
  type witness
  class type js = object
    method geometry_witness : witness
    method faces : Face3.t js_array Js.t prop
    (* if you change faces, set these to true. *)
    method elementsNeedUpdate : bool Js.t prop
    method colorsNeedUpdate : bool Js.t prop
    method normalsNeedUpdate : bool Js.t prop

    method vertices : Vector3.t js_array Js.t prop
    method verticesNeedUpdate : bool Js.t prop

    (* A 3D array:
       geometry.faceVertexUvs[ materialIndex ][ faceIndex ][ vertexIndex ]
    *)
    method faceVertexUvs
      : Vector2.t
      js_array Js.t
      js_array Js.t
      js_array Js.t prop
    method uvsNeedUpdate : bool Js.t prop
  end

  type t = js Js.t

  val create : unit -> t
end

module Material : sig
  module Side : sig
    type t
    val front_side  : t Js.t
    val back_side   : t Js.t
    val double_side : t Js.t
  end

  type witness
  class type js = object
    method material_witness : witness
    method side : Side.t Js.t prop
    method transparent : bool Js.t prop
  end

  type t = js Js.t

  val create : unit -> t
end

module MeshBasicMaterial : sig
  type witness
  class type js = object
    inherit Material.js
    method mesh_basic_material_witness : witness
    method wireframe : bool Js.t prop
  end

  type t = js Js.t

  val create
    :  ?color:Color.t
    -> ?map:Texture.t
    -> unit
    -> t
end

module Object3D : sig
  type witness
  class type js = object
    method object3d_witness : witness
    (* If [matrixAutoUpdate] is [true], this gets recomputed from position,
       quaternion, and scale, so setting this has no effect. *)
    method matrix : Matrix4.t prop
    method matrixAutoUpdate : bool Js.t prop
    (* This only takes the position, rotation, and scale of the matrix. Any
       projective part is thrown away. *)
    method applyMatrix : Matrix4.t -> unit meth
    method add : js Js.t -> unit meth
  end

  type t = js Js.t

  val create : unit -> t
end

module Mesh : sig
  type witness
  class type js = object
    inherit Object3D.js
    method mesh_witness : witness
  end

  type t = js Js.t

  val create : Geometry.t -> Material.t -> t
end

module Camera : sig
  type witness
  class type js = object
    inherit Object3D.js
    method camera_witness : witness
  end

  type t = js Js.t

  val create : unit -> t
end

module OrthographicCamera : sig
  type witness
  class type js = object
    inherit Camera.js
    method orthographic_camera_witness : witness
  end

  type t = js Js.t

  val create
    :  left:float
    -> right:float
    -> top:float
    -> bottom:float
    -> near:float
    -> far:float
    -> t
end

module Scene : sig
  type witness
  class type js = object
    inherit Object3D.js
    method scene_witness : witness
  end

  type t = js Js.t

  val create : unit -> t
end

module WebGLRenderer : sig
  type witness
  class type js = object
    method webgl_renderer_witness : witness
    method setSize : width:float -> height:float -> updateStyle:bool Js.t -> unit meth
    method setClearColor : Color.t -> unit meth
    method domElement : Html.canvasElement Js.t readonly_prop
    method render : Scene.t -> Camera.t -> unit meth
  end

  type t = js Js.t

  val create : unit -> t
end
