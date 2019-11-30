open Core

module V = Vector.Float
module P = V
module Poly = Polynomial

module T = struct
  type t = P.t * V.t [@@deriving sexp, compare]
end

include T

let mem (p0, v) p =
  V.(cross (p - p0) v) = 0.

let poly (p0, v) =
  let open P in
  Polynomial.zero_line_between_two_points p0 (p0 + v)

let deform_to_zero (l : t) p =
  let open Polynomial in
  let ((x, y), (d_x, d_y)) = l in
  let v, x =
    match d_x, d_y with
    | 0., _ -> Var.x, x
    | _, 0. -> Var.y, y
    | _ -> failwith "emerge only works on horizontal or vertical lines"
  in
  p - subst p ~var:v ~by:(const x)

let deform_to_zero_preserving_boundary t config p =
  let open Polynomial in
  let vert = (0., 1.) in
  let horz = (1., 0.) in
  let (x0, y0), (x1, y1) = Config.domain config in
  let ((x, y), (d_x, d_y)) = t in
  let v, x, l1, l2 =
    match d_x, d_y with
    | 0., _ -> Var.x, x, ((x0, 0.), vert), ((x1, 0.), vert)
    | _, 0. -> Var.y, y, ((0., y0), horz), ((0., y1), horz)
    | _ -> failwith "emerge only works on horizontal or vertical lines"
  in
  let l1 = poly l1 in
  let l2 = poly l2 in
  let env = l1 * l2 in
  let env_value = eval_point env (x, y) in
  let env = scale env ~by:(1. /. env_value) in
  p - (subst p ~var:v ~by:(const x) * env)

include Comparable.Make(T)
