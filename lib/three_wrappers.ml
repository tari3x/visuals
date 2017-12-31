open Base
open Js
open Lwt
open Common
open Geometry

module Vector2 = struct
  include Three.Vector2

  let of_vector v =
    let (x, y) = Vector.coords v in
    create x y
end

module Vector3 = struct
  include Three.Vector3

  let of_vector v =
    let (x, y) = Vector.coords v in
    create x y 0.
end

module Matrix4 = struct
  include Three.Matrix4

  let of_matrix m =
    let t = create () in
    let c = Matrix.get m in
    t##set
      (c 0 0) (c 0 1) 0. (c 0 2)
      (c 1 0) (c 1 1) 0. (c 1 2)
      0.      0.      1. 0.
      (c 2 0) (c 2 1) 0. (c 2 2);
    t

  let identity () =
    let t = create () in
    t##identity;
    t
end

module Color = struct
  include Three.Color

  let red   = create 1. 0. 0.
  let green = create 0. 1. 0.
  let blue  = create 0. 0. 1.
end


module Texture = struct
  include Three.Texture

  let load_image
      ?(min_filter = MinFilter.linear)
      path =
    let loader = Three.TextureLoader.create () in
    Lwt.wrap (fun c ->
      loader##load (Js.string path) (Js.wrap_callback c))
    >>= fun t ->
    t##.minFilter := min_filter;
    return t

  let load_video
      ?(min_filter = MinFilter.linear)
      ?loop
      path =
    let open Dom_wrappers in
    let video = Video.load ?loop path in
    let t = Three.VideoTexture.create video in
    t##.minFilter := min_filter;
    Lwt.return t
end

module Face = struct
  class type js = object
    inherit Three.Face3.js
    method uvs : Three.Vector2.t js_array Js.t prop
  end

  type t = js Js.t

  let create
      ?(color = Three.Color.create 1. 1. 1.)
      (* CR-someday: just call [computeFaceNormals] *)
      ?(normal = Vector3.create 1. 1. 1.)
      ~uvs:(uv1, uv2, uv3)
      (i1, i2, i3) =
    let uvs =
      [| uv1; uv2; uv3 |]
      |> Array.map ~f:Vector2.of_vector
      |> Js.array
    in
    (* CR-someday: with some syntax support from js_of_ocaml it should be
       possible to avoid [Obj.magic]. js_of_ocaml should just allow to expose a
       class in an inheritable way. *)
    let (t : t) =
      Three.Face3.create i1 i2 i3 ~normal color ~material_index:0
      |> Caml.Obj.magic
    in
    t##.uvs := uvs;
    t
end

module Geometry = struct
  include Three.Geometry

  let update_faces (t : t) (faces : Face.t array) =
    let uvs =
      Array.map faces ~f:(fun face -> face##.uvs)
      |> Js.array
    in
    let faces =
      Array.map faces ~f:(fun face -> (face :> Three.Face3.t))
      |> Js.array
    in
    t##.faces := faces;
    t##.elementsNeedUpdate := Js._true;
    t##.colorsNeedUpdate := Js._true;
    t##.normalsNeedUpdate := Js._true;
    t##.faceVertexUvs := Js.array [| uvs |];
    t##.uvsNeedUpdate := Js._true

  let create ~vertices ~faces () =
    let t = create () in
    t##.vertices := Js.array vertices;
    t##.verticesNeedUpdate := Js._true;
    update_faces t faces;
    t
end

module Object3D = struct
  include Three.Object3D

  let set_matrix t m =
    t##.matrixAutoUpdate := Js._false;
    t##.matrix := m
end

module OrthographicCamera = struct
  include Three.OrthographicCamera

  let create_with_body_size () =
    create
      ~left:0.
      ~right:(float Html.document##.body##.clientWidth)
      ~top:0.
      ~bottom:(float Html.document##.body##.clientHeight)
      ~near:0.
      ~far:1000.
end

module WebGLRenderer = struct
  include Three.WebGLRenderer

  let create_with_body_size () =
    let t = create () in
    t##setSize
      ~width:(float Html.document##.body##.clientWidth)
      ~height:(float Html.document##.body##.clientHeight)
      ~updateStyle:Js._true;
    t
end
