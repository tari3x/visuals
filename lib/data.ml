open Core
open Async
open Std_internal

module L = P.Lagrange

type t =
  { data : P.Data.t
  ; lagrange : L.t sexp_opaque
  ; desc : Sexp.t option
  ; show_dots : V.t list
  } [@@deriving sexp]

let create data ~degree =
  let basis = P.Basis.Kind.mono ~degree in
  (*
  let (v1, v2) = Config.domain config in
  let v_margin = (1., 1.) in
  let v1 = V.(v1 - v_margin) in
  let v2 = V.(v2 + v_margin) in
  let basis = P.Basis.Kind.bernstein ~degree ~domain:(v1, v2) in
  *)
  let lagrange = L.create data ~basis ~size_unused:0 in
  { data; lagrange; desc = None; show_dots = [] }

let add_data t ~data =
  let lagrange = L.add_data t.lagrange ~data in
  let data = t.data @ data in
  { data; lagrange; desc = t.desc; show_dots = t.show_dots }

let set_desc t ~desc =
  { t with desc = Some desc }

let _set_dots t ~dots =
  { t with show_dots = dots }

