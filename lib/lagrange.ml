open Core
open Std_internal

(* "On multivariate Lagrange interpolation" by Thomas Sauer and Yuan Xu. *)

let debug a = debug_s ~enabled:false a

module Datum = struct
  type t = V.t * float [@@deriving sexp]

  let weighted_average ((x1, y1), v1) ((x2, y2), v2) ~w =
    let x = weighted_average x1 x2 ~w in
    let y = weighted_average y1 y2 ~w in
    let v = weighted_average v1 v2 ~w in
    ((x, y), v)
end

module Data = struct
  type t = Datum.t list [@@deriving sexp]
end

let error t data =
  let open Float in
  List.map data ~f:(fun ((x, y), value) ->
    abs (P.eval t [x; y] - value))
  |> Float.sum


(* Invariant:
   1. [ps @ qs] are a basis.
   2. [ps] are orthonormal w.r.t [vs]
   3. [qs] are 0 in all [vs].
*)
type t =
  { ps : P.t list
  ; qs : P.t list
  ; vs : Datum.t list
  } [@@deriving sexp_of]

type self = t [@@deriving sexp_of]

let init basis =
  { qs = basis; ps = []; vs = [] }

let balanced_q qs ~ev =
  let open Float in
  (* let min_weight = 1. in *)
  let qs =
    List.map qs ~f:(fun q ->
      (ev q, q))
  in
  (* CR: probably not necessary. *)
  let w_abs_tot =
    List.map qs ~f:(fun (w, _) -> Float.abs w)
    |> Float.sum
  in
  debug [%message (qs : (float * P.t) list)];
  List.map qs ~f:(fun (w, q) -> P.scale q ~by:w)
  |> P.sum
  |> P.scale ~by:(1. / w_abs_tot)

let add_point { ps; qs; vs } v =
  let open Float in
  let (x, y) = fst v in
  let ev q = P.eval q [x; y] in
  let num_points = List.length vs in
  let next_q qs =
    match qs with
    | [] -> failwithf "max_num_points too low %d points" num_points ()
    | qs  ->
      let q = balanced_q qs ~ev in
      debug [%message (q : P.t)];
      if ev q = 0.
      then failwith "balanced q is zero";
      q, List.tl_exn qs
  in
  let vs = v :: vs in
  let q, qs = next_q qs in
  let new_p = P.(scale q ~by:(1. /. ev q)) in
  let adjust p = P.(p - scale new_p ~by:(ev p)) in
  let ps = List.map ps ~f:adjust in
  let qs = List.map qs ~f:adjust in
  let ps = new_p :: ps in
  { ps; qs; vs }

let add_data t ~data =
  List.fold data ~init:t ~f:(fun t v ->
    let t = add_point t v in
    debug [%message (t : t)];
    t)

let create ~basis data =
  init basis
  |> add_data ~data

let result { ps; vs; qs = _ } =
  let values = List.map vs ~f:snd in
  List.map2_exn ps values ~f:(fun p w -> P.scale p ~by:w)
  |> P.sum

let simple ~basis data =
  create ~basis data
  |> result
