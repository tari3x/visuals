open Js
open Util
open Common
open Geometry

module Vector2 : sig
  include module type of struct include Three.Vector2 end

  val of_vector : Vector.t -> t
end

module Vector3 : sig
  include module type of struct include Three.Vector3 end

  val of_vector : Vector.t -> t
end

module Matrix4 : sig
  include module type of struct include Three.Matrix4 end

  val of_matrix : Matrix.t -> t
  val identity : unit -> t
end

module Color : sig
  include module type of struct include Three.Color end

  val red   : t
  val green : t
  val blue  : t
end

module Texture : sig
  include module type of struct include Three.Texture end

  val load_image
    :  ?min_filter:MinFilter.t Js.t
    -> string
    -> t Lwt.t

  val load_video
    :  ?min_filter:MinFilter.t Js.t
    -> ?loop:bool
    -> string
    -> t Lwt.t
end

(*
module Render_target : sig
  type t

  val webgl : height:float -> width:float -> t

  val texture : t -> Texture.t
end
*)

module Face : sig
  class type js = object
    inherit Three.Face3.js
    method uvs : Three.Vector2.t js_array Js.t prop
  end

  type t = js Js.t

  val create
    :  ?color:Three.Color.t
    -> ?normal:Vector3.t
    -> uvs:(Vector.t * Vector.t * Vector.t)
    -> (int * int * int)
    -> t
end

module Geometry : sig
  include module type of struct include Three.Geometry end

  val create
    :  vertices:Vector3.t array
    -> faces:Face.t array
    -> unit
    -> t

  val update_faces : t -> Face.t array -> unit
end

module Object3D : sig
  include module type of struct include Three.Object3D end

  val set_matrix : t -> Matrix4.t -> unit
end

module OrthographicCamera : sig
  include module type of struct include Three.OrthographicCamera end

  (* CR: will this flip upside down? *)
  (* Use the camera with same size and aspect ratio as the document body to
     avoid the extra layer of conversion. *)
  val create_with_body_size : unit -> t
end

module WebGLRenderer : sig
  include module type of struct include Three.WebGLRenderer end

  val create_with_body_size : unit -> t
end
