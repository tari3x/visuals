(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)


module Angle : sig
  type t

  val to_string : t -> string

  val zero : t

  val of_degrees : float -> t
  val of_radians : float -> t
  val to_radians : t -> float

  val neg : t -> t

  val (-) : t -> t -> t
  val (+) : t -> t -> t
end

module Vector : sig
  type t [@@deriving sexp]

  val create : int -> int -> t
  val create_float : float -> float -> t
  val coords : t -> (float * float)
  val x : t -> float
  val y : t -> float

  val zero : t

  val length : t -> float
  val dir : t -> Angle.t
  val angle : t -> t -> Angle.t
  val normalize : t -> t

  val neg : t -> t
  val (-) : t -> t -> t
  val (+) : t -> t -> t

  val scale : t -> by:float -> t
  val ( / ) : t -> float -> t
  val ( * ) : t -> float -> t

  val random_unit : unit -> t

  val to_string : t -> string
end

module Matrix : sig
  type t

  val create
    :  float * float * float
    -> float * float * float
    -> float * float * float
    -> t

  (* 0-based, row-major *)
  val get : t -> int -> int -> float

  val to_string : t -> string

  val ident : t

  val ( * )   : t -> t -> t
  val ( *> )  : t -> t -> t

  val inv  : t -> t

  val translate : Vector.t -> t
  val rotate : Angle.t -> t
  val scale : scale_x:float -> scale_y:float -> t

  val apply : t -> Vector.t -> Vector.t

  val coeffs : t -> float array array

  val transpose_exn : t -> t

  val suitable_for_context2d_exn : t -> unit
end

module Frame : sig
  type t [@@deriving sexp]

  val to_string : t -> string

  val ( *> )  : t -> t -> t

  val ident : t
  val translate : Vector.t -> t
  val rotate : Angle.t -> t
  val scale : scale_x:float -> scale_y:float -> t

  val scale_viewport : t -> float -> t

  val remove_scale : t -> t

  val equal_scale : t -> t

  val translation : t -> Vector.t
  val set_translation : t -> Vector.t -> t

  val scale_x : t -> float

  val matrix : t -> Matrix.t
end

module Rectangle : sig
  module V = Vector
  type t

  val create_corners : V.t -> V.t -> t
  val create_offset : V.t -> width:float -> height:float -> t
  val top_left : t -> V.t
  val width : t -> float
  val height : t -> float
  val corners : t -> V.t * V.t * V.t * V.t
end

module Shape : sig
  type t =
  | Segment of Vector.t * Vector.t
  | Path of Vector.t list
  | Polygon of Vector.t list (* not empty *)
      [@@deriving sexp]

  val segment : Vector.t -> Vector.t -> t
  val polygon : Vector.t list -> t
  val path : Vector.t list -> t
end
