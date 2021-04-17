open Bigarray

(* Re max size:
   https://github.com/KhronosGroup/OpenGL-API/issues/36
*)

module type T = sig
  type t

  val glsl_declaration : t -> name:string -> string
end

module Scalar : sig
  include T

  (** Dims must match the shape of the data, otherwise [update] raises. *)
  val create_empty : _ Bigarray.kind -> dims:int array -> t

  val create1 : (_, _, c_layout) Array1.t -> t
  val create2 : (_, _, c_layout) Array2.t -> t
  val create3 : (_, _, c_layout) Array3.t -> t
  val update1 : t -> (_, _, c_layout) Array1.t -> unit
  val update2 : t -> (_, _, c_layout) Array2.t -> unit
  val update3 : t -> (_, _, c_layout) Array3.t -> unit
end

module Color : sig
  include T

  (** Length must match the shape of the data, otherwise [update] raises. *)
  val create_empty : length:int -> t

  val create : Color.t array -> t
  val update : t -> Color.t array -> unit
end
