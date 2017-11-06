(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common

module Angle = struct
  type t = float

  let to_string t =
    Printf.sprintf "%f rad" t

  let zero = 0.

  let of_radians t = t
  let of_degrees d = d *. (pi /. 180.)
  let to_radians t = t

  let neg t = -. t

  let (-) = (-.)
  let (+) = (+.)
end

module Vector = struct
  type t = (float * float * float)

  let create_float x y =
    (x, y, 1.)

  let create x y =
    create_float (float x) (float y)

  let projective_coords t = t

  let of_projective_coords (x, y, z) =
    ( x /. z, y /. z, 1.)

  let coords (x, y, _) =
    (x, y)

  let x (x, _, _) = x
  let y (_, y, _) = y

  let zero =
    create_float 0. 0.

  let length (x, y, _) =
    Float.hypot x y

  let cross (x1, y1, _) (x2, y2, _) =
    x1 *. y2 -. x2 *. y1

  let dot (x1, y1, _) (x2, y2, _) =
    x1 *. x2 +. y1 *. y2

  let dir (x, y, _) =
    Float.atan2 x y

  let angle v1 v2 =
    Float.atan2 (cross v1 v2) (dot v1 v2)

  let neg (x, y, _) =
    create_float (-. x) (-. y)

  let (-) t1 t2 =
    let x = x t1 -. x t2 in
    let y = y t1 -. y t2 in
    create_float x y

  let (+) t1 t2 =
    let x = x t1 +. x t2 in
    let y = y t1 +. y t2 in
    create_float x y

  let scale (x, y, z) ~by =
    (x *. by, y *. by, z)

  let ( * ) v x =
    scale v ~by:x

  let ( / ) v x =
    scale v ~by:(1. /. x)

  let to_string (x, y, _) =
    Printf.sprintf "(%f, %f)" x y
end

module Matrix = struct
  include Math.Matrix

  let create
      ( x1, x2, x3)
      ( y1, y2, y3)
      ( z1, z2, z3)
      =
    of_array
      [| [| x1; x2; x3|]
      ;  [| y1; y2; y3|]
      ;  [| z1; z2; z3|]
      |]

  let get (t : t) i j =
    t
    |> Fn.flip Js.array_get i
    |> Optdef.value_exn
    |> Fn.flip Js.array_get j
    |> Optdef.value_exn

  let to_string (t : t) =
    let c = get t in
    Printf.sprintf
      "[ %f %f %f ]; \
       [ %f %f %f ]; \
       [ %f %f %f ]"
      (c 0 0) (c 0 1) (c 0 2)
      (c 1 0) (c 1 1) (c 1 2)
      (c 2 0) (c 2 1) (c 2 2)

  let ( * ) = Math.multiply
  let inv   = Math.inv

  let ( *> ) t1 t2 = t2 * t1

  let translate v : t =
    let (x, y) = Vector.coords v in
    of_array
      [| [| 1.; 0.; x |]
      ;  [| 0.; 1.; y |]
      ;  [| 0.; 0.; 1. |]
      |]

  let scale ~scale_x:x ~scale_y:y : t =
    of_array
      [| [| x;  0.; 0. |]
      ;  [| 0.; y;  0. |]
      ;  [| 0.; 0.; 1. |]
      |]

  let rotate a : t =
    let open Float in
    let a = Angle.to_radians a in
    of_array
      [| [| cos a;      sin a; 0. |]
      ;  [| -. (sin a); cos a; 0. |]
      ;  [| 0.;         0.;    1. |]
      |]

  let ident =
    translate Vector.zero

  let apply (t : t) v  =
    let (v0, v1, v2) = Vector.projective_coords v in
    let c = get t in
    let coord i =
      c i 0 *. v0
      +. c i 1 *. v1
      +. c i 2 *. v2
    in
    Vector.of_projective_coords ( coord 0, coord 1, coord 2 )

  let coeffs =
    Math.Matrix.to_array

  let suitable_for_context2d_exn t =
    let c = get t in
    if Float.equal (c 2 0) 0.
      && Float.equal (c 2 1) 0.
      && Float.equal (c 2 2) 1.
    then ()
    else failwithf "%s not suitable for context2d" (to_string t) ()

  let transpose_exn t =
    to_array t
    |> Array.transpose_exn
    |> of_array
end

module Frame = struct
  type t =
    { scale_x : float
    ; scale_y : float
    ; rotation : Angle.t
    ; translation : Vector.t
    }

  let ( *> ) t1 t2 =
    { scale_x = t1.scale_x *. t2.scale_x
    ; scale_y = t1.scale_y *. t2.scale_y
    ; rotation = Angle.(t1.rotation + t2.rotation)
    ; translation = Vector.(t1.translation + t2.translation)
    }

  let ident =
    { scale_x = 1.
    ; scale_y = 1.
    ; rotation = Angle.zero
    ; translation = Vector.zero
    }

  let translate translation =
    { ident with translation }

  let rotate rotation =
    { ident with rotation }

  let scale ~scale_x ~scale_y =
    { ident with scale_x; scale_y }

  let set_translation t translation =
    { t with translation }

  let translation t =
    t.translation

  let scale_viewport t scale =
    { t with
      scale_x = t.scale_x *. scale
      ; scale_y = t.scale_y *. scale
      ; translation = Vector.scale t.translation ~by:scale
    }

  let scale_x t =
    t.scale_x

  let remove_scale t =
    { t with
      scale_x = 1.
      ; scale_y = 1.
    }

  let equal_scale t =
    let scale = Float.max t.scale_x t.scale_y in
    { t with
      scale_x = scale
      ; scale_y = scale
    }

  let matrix t =
    Matrix.(
      scale ~scale_x:t.scale_x ~scale_y:t.scale_y
      *> rotate t.rotation
      *> translate t.translation)

  let to_string t =
    Matrix.to_string (matrix t)
end

