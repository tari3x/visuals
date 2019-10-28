open Base
open Js_of_ocaml
open Js

module Html = Dom_html

module Vector2 = struct
  type witness
  class type js = object
    method vector2_witness : witness
  end

  type t = js Js.t

  let constr : (float -> float -> t) constr =
    Js.Unsafe.global##.THREE##.Vector2

  let create x y =
    new%js constr x y
end

module Vector3 = struct
  type witness
  class type js = object
    method vector3_witness : witness
  end

  type t = js Js.t

  let constr : (float -> float -> float -> t) constr =
    Js.Unsafe.global##.THREE##.Vector3

  let create x y z =
    new%js constr x y z
end

module Matrix4 = struct
  type witness
  class type js = object
    method matrix4_witness : witness
    (* row-major, but internally stored as column-major (see [toArray]). Wut? *)
    method set
      : float -> float -> float -> float
        -> float -> float -> float -> float
          -> float -> float -> float -> float
            -> float -> float -> float -> float
              -> unit meth
    method elements : float js_array Js.t readonly_prop
    method identity : unit meth
  end

  type t = js Js.t

  let constr : t constr =
    Js.Unsafe.global##.THREE##.Matrix4

  let create () =
    new%js constr
end

module Color = struct
  type witness
  class type js = object
    method color_witness : witness
  end

  type t = js Js.t

  let constr : (float -> float -> float -> t) constr =
    Js.Unsafe.global##.THREE##.Color

  let create r g b =
    new%js constr r g b
end

module Texture = struct
  module MinFilter = struct
    type t

    let e = Js.Unsafe.eval_string

    let nearest                : t Js.t = e "THREE.NearestFilter"
    let nearest_mipmap_nearest : t Js.t = e "THREE.NearestMipMapNearestFilter"
    let nearest_mipmap_linear  : t Js.t = e "THREE.NearestMipMapLinearFilter"
    let linear                 : t Js.t = e "THREE.LinearFilter"
    let linear_mipmap_nearest  : t Js.t = e "THREE.LinearMipMapNearestFilter"
    let linear_mipmap_linear   : t Js.t = e "THREE.LinearMipMapLinearFilter"
  end

  type witness
  class type js = object
    method texture_witness : witness
    method minFilter : MinFilter.t Js.t prop
  end

  type t = js Js.t
end

module TextureLoader = struct
  type witness
  class type js = object
    method texture_loader_witness : witness
    method load : js_string Js.t -> (Texture.t -> unit) callback -> unit meth
  end

  type t = js Js.t

  let constr : t constr =
    Js.Unsafe.global##.THREE##.TextureLoader

  let create () =
    new%js constr
end

module VideoTexture = struct
  type witness
  class type js = object
    inherit Texture.js
  end

  type t = js Js.t

  let constr : (Html.videoElement Js.t -> t) constr =
    Js.Unsafe.global##.THREE##.VideoTexture

  let create video =
    new%js constr video
end

module Face3 = struct
  type witness
  class type js = object
    method face3_witness : witness
  end

  type t = js Js.t

  let constr : (int -> int -> int -> Vector3.t -> Color.t -> int -> t) constr =
    Js.Unsafe.global##.THREE##.Face3

  let create i1 i2 i3 ~normal c ~material_index =
    new%js constr i1 i2 i3 normal c material_index
end

module Geometry = struct
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

    method faceVertexUvs
      : Vector2.t
      js_array Js.t
      js_array Js.t
      js_array Js.t prop
    method uvsNeedUpdate : bool Js.t prop
  end

  type t = js Js.t

  let constr : t constr =
    Js.Unsafe.global##.THREE##.Geometry

  let create () =
    new%js constr
end

module Material = struct
  module Side = struct
    type t

    let e = Js.Unsafe.eval_string

    let front_side  : t Js.t = e "THREE.FrontSide"
    let back_side   : t Js.t = e "THREE.BackSide"
    let double_side : t Js.t = e "THREE.DoubleSide"
  end

  type witness
  class type js = object
    method material_witness : witness
    method side : Side.t Js.t prop
    method transparent : bool Js.t prop
  end

  type t = js Js.t

  let constr : t constr =
    Js.Unsafe.global##.THREE##.Material

  let create () =
    new%js constr
end

module MeshBasicMaterial = struct
  type witness
  class type js = object
    inherit Material.js
    method mesh_basic_material_witness : witness

    method wireframe : bool Js.t prop
  end

  type t = js Js.t

  module Params = struct
    class type js = object
      method color : Color.t optdef_prop
      method map : Texture.t optdef_prop
    end

    type t = js Js.t

    let create ?color ?map () =
      (* CR-someday: Js.Unsafe.coerce *)
      let t : t = Caml.Obj.magic (object%js end) in
      Option.iter color ~f:(fun color -> t##.color := color);
      Option.iter map ~f:(fun map -> t##.map := map);
      t
  end

  let constr : (Params.t -> t) constr =
    Js.Unsafe.global##.THREE##.MeshBasicMaterial

  let create ?color ?map () =
    new%js constr (Params.create ?color ?map ())
end

module Object3D = struct
  type witness
  class type js = object
    method object3d_witness : witness
    method matrix : Matrix4.t prop
    method matrixAutoUpdate : bool Js.t prop
    (* This only takes the position, rotation, and scale of the matrix. *)
    method applyMatrix : Matrix4.t -> unit meth
    method add : js Js.t -> unit meth
  end

  type t = js Js.t

  let constr : t constr =
    Js.Unsafe.global##.THREE##.Object3D

  let create () =
    new%js constr
end

module Mesh = struct
  type witness
  class type js = object
    inherit Object3D.js
    method mesh_witness : witness
  end

  type t = js Js.t

  let constr : (Geometry.t -> Material.t -> t) constr =
    Js.Unsafe.global##.THREE##.Mesh

  let create g m =
    new%js constr g m
end

module Camera = struct
  type witness
  class type js = object
    inherit Object3D.js
    method camera_witness : witness
  end

  type t = js Js.t

  let constr : t constr =
    Js.Unsafe.global##.THREE##.Camera

  let create () =
    new%js constr
end

module OrthographicCamera = struct
  type witness
  class type js = object
    inherit Camera.js
    method orthographic_camera_witness : witness
  end

  type t = js Js.t

  let constr : (float -> float -> float -> float -> float -> float -> t) constr =
    Js.Unsafe.global##.THREE##.OrthographicCamera

  let create ~left ~right ~top ~bottom ~near ~far =
    new%js constr left right top bottom near far
end

module Scene = struct
  type witness
  class type js = object
    inherit Object3D.js
    method scene_witness : witness
  end

  type t = js Js.t

  let constr : t constr =
    Js.Unsafe.global##.THREE##.Scene

  let create () =
    new%js constr
end

module WebGLRenderer = struct
  type witness
  class type js = object
    method webgl_renderer_witness : witness
    method setSize : width:float -> height:float -> updateStyle:bool Js.t -> unit meth
    method setClearColor : Color.t -> unit meth
    method domElement : Html.canvasElement Js.t readonly_prop
    method render : Scene.t -> Camera.t -> unit meth
  end

  type t = js Js.t

  let constr : t constr =
    Js.Unsafe.global##.THREE##.WebGLRenderer

  let create () =
    new%js constr
end
