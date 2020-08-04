open Js_of_ocaml
open Js
open Geometry
module T = Three_lib

include module type of struct
  include T
end

module Vector2 : sig
  include module type of struct
    include T.Vector2
  end

  val of_vector : Vector.t -> t
end

module Vector3 : sig
  include module type of struct
    include T.Vector3
  end

  val of_vector : Vector.t -> t
end

module Matrix4 : sig
  include module type of struct
    include T.Matrix4
  end

  val of_matrix : Matrix.t -> t
  val identity : unit -> t
end

module Color : sig
  include module type of struct
    include T.Color
  end

  val red : t
  val green : t
  val blue : t
end

module Texture : sig
  include module type of struct
    include T.Texture
  end

  val load_image : ?min_filter:MinFilter.t Js.t -> string -> t Lwt.t

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
  class type js =
    object
      inherit T.Face3.js

      method uvs : T.Vector2.t js_array Js.t prop
    end

  type t = js Js.t

  val create
    :  ?color:T.Color.t
    -> ?normal:Vector3.t
    -> uvs:Vector.t * Vector.t * Vector.t
    -> int * int * int
    -> t
end

module Geometry : sig
  include module type of struct
    include T.Geometry
  end

  val create : vertices:Vector3.t array -> faces:Face.t array -> unit -> t
  val update_faces : t -> Face.t array -> unit
end

module Object3D : sig
  include module type of struct
    include T.Object3D
  end

  val set_matrix : t -> Matrix4.t -> unit
end

module OrthographicCamera : sig
  include module type of struct
    include T.OrthographicCamera
  end

  (* CR: will this flip upside down? *)
  (* Use the camera with same size and aspect ratio as the document body to
     avoid the extra layer of conversion. *)
  val create_with_body_size : unit -> t
end

module WebGLRenderer : sig
  include module type of struct
    include T.WebGLRenderer
  end

  val create_with_body_size : unit -> t
end
