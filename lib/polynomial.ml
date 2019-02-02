open Core
open Bigarray

open Common

module E = Maxima.Expr
module V = Vector.Float
module Matrix = Maxima.Matrix
module A2 = Array2

let _debug a = debug ~enabled:true a

(* CR-someday: this will become [Atom] which is either variable with index or
   arbitrary string. *)
module Var = struct
  include String

  let create n =
    E.(var n |> to_string)

  let call name =
    sprintf "%s(%s, %s)" name (create 1) (create 2)

  let verbatim t = t
end

(* CR-someday: massively inefficient. Just specialize to 2D. *)
module Values = struct
  type t = float Var.Map.t

  let create values : t =
    List.mapi values ~f:(fun i v -> Var.create Int.(i + 1), v)
    |> Var.Map.of_alist_exn

  let find_exn = Map.find_exn
end

module Mono = struct
  module T = struct
    type t = (Var.t * int) list
    [@@deriving sexp, compare, hash]
  end

  include T

  let one : t = []

  let is_one = List.is_empty

  let var n : t =
    [Var.create n, 1]

  let call name : t =
    [Var.call name, 1]

  let verbatim s : t =
    [Var.verbatim s, 1]

  (* Going via map keeps variables sorted. *)
  let ( * ) (t1 : t) (t2 : t) : t =
    Var.Map.of_alist_multi (t1 @ t2)
    |> Map.to_alist
    |> List.map ~f:(fun (var, powers) ->
      var, Int.sum powers)

  let pow t n =
    if n = 0 then one
    else List.map t ~f:(fun (v, m) -> (v, Int.(n * m)))

  let to_maxima t =
    List.map t ~f:(fun (v, n) ->
      E.(pow (of_string v) n))
    |> E.product

  let eval (t : t) (values : Values.t) =
    List.map t ~f:(fun (v, n) ->
      Float.int_pow (Values.find_exn values v) n)
    |> Float.product

  let all_of_degree n =
    List.init Int.(n + 1) ~f:(fun i ->
      let j = Int.(n - i) in
      (pow (var 1) i *  pow (var 2) j))

  let all ~degree:n =
    List.init Int.(n + 1) ~f:(fun i ->
      List.init Int.(n + 1) ~f:(fun j ->
        if Int.(i + j > n) then None
        else Some (pow (var 1) i *  pow (var 2) j))
      |> List.filter_opt)
    |> List.concat

  (*
    let all ~degree:n =
    List.init Int.(n + 1) ~f:all_of_degree
    |> List.concat
  *)

  let first n =
    let rec aux ~degree n =
      let ts = all_of_degree degree in
      let nd = List.length ts in
      if n <= nd
      then List.take ts n
      else ts @ aux ~degree:(degree + 1) (n - nd)
    in
    aux ~degree:0 n

  include Comparable.Make(T)
  include Hashable.Make(T)
end

type t = (Mono.t * float) list

let zero = []

let const x =
  [Mono.one, x]

let mono m =
  [m, 1.]

let var n =
  mono (Mono.var n)

let call name =
  mono (Mono.call name)

let verbatim s =
  mono (Mono.verbatim s)

let group_sum t =
  Mono.Map.of_alist_multi t
  |> Map.to_alist
  |> List.filter_map ~f:(fun (mono, coeffs) ->
    let c = Float.sum coeffs in
    if Float.(c = 0.) then None
    else Some (mono, c))

let ( + ) (t1 : t) (t2 : t) : t =
  group_sum (t1 @ t2)

let ( * ) (t1 : t) (t2 : t) : t =
  List.cartesian_product t1 t2
  |> List.map ~f:(fun ((m1, c1), (m2, c2)) ->
    (Mono.(m1 * m2), Float.(c1 * c2)))
  |> group_sum

let product = function
  | [] -> (const 1.)
  | ts -> List.reduce_exn ts ~f:( * )

let sum = function
  | [] -> zero
  | ts -> List.reduce_exn ts ~f:(+)

(* CR-someday: make efficient. *)
let pow t n =
  List.init n ~f:(fun _ -> t)
  |> product

let scale t x =
  product [ const x; t ]

let ( - ) t1 t2 =
  sum [t1; scale t2 (-1.)]

let var_x = var 1
let var_y = var 2

let to_maxima (t : t) =
  List.map t ~f:(fun (m, c) ->
    if Mono.is_one m then E.const c
    else begin
      let m = Mono.to_maxima m in
      if Float.(c = 1.) then m
      else E.(const c * m)
    end)
  |> E.sum

let to_string t =
  to_maxima t |> E.to_string

let sexp_of_t t =
  to_string t |> String.sexp_of_t

let to_gnuplot t =
  to_maxima t
  |> E.to_gnuplot

let zero_line_between_two_points (x1, y1) (x2, y2) =
  if Float.(x2 = x1)
  then var_x - const x1
  else begin
    let slope = Float.((y2 - y1) / (x2 - x1)) in
    var_y - const y1 - const slope * (var_x - const x1)
  end

let monomials = List.map ~f:List.return

let eval (t : t) (values : float list) =
  let values = Values.create values in
  List.map t ~f:(fun (m, c) ->
    Float.(c * Mono.eval m values))
  |> Float.sum

let eval_point (t : t) (x, y) =
  eval t [x; y]

(* 2D only *)
let all_monomials ~degree =
  Mono.all ~degree |> List.map ~f:mono

let first_monomials n =
  Mono.first n |> List.map ~f:mono

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
    abs (eval t [x; y] - value))
  |> Float.sum

module Basis = struct
  type t =
    { degree : int
    ; size : int
    }
end

(* "On multivariate Lagrange interpolation" by Thomas Sauer and Yuan Xu. *)
module Lagrange = struct
  type poly = t [@@deriving sexp_of]

  type t =
    { ps : poly list
    ; qs : poly list
    ; vs : Datum.t list
    } [@@deriving sexp_of]

  (* CR-someday: the paper suggests a different basis which is more stable. *)
  let init {Basis. degree; size} =
    let qs = List.take (all_monomials ~degree) size in
    { qs; ps = []; vs = [] }

  let add_point ({ ps; qs; vs } as t) v =
    let (x, y) = fst v in
    let eval q = eval q [x; y] in
    (* CR-someday: don't eval multiple times. *)
    let qs =
      List.sort qs ~compare:(fun q1 q2 ->
        Float.compare (Float.abs (eval q1)) (Float.abs (eval q2)))
      |> List.rev
    in
    let vs = v :: vs in
    let num_points = List.length vs in
    match qs with
    | [] -> failwithf "max_num_points too low %d points" num_points ()
    | q :: qs ->
      let q_val = eval q in
      if Float.(q_val = 0.)
      then failwithf
        !"No unique interpolation: %{sexp:t}, %{sexp:Datum.t}" t v ();
      let new_p = scale q (1. /. q_val) in
      let adjust p = p - scale new_p (eval p) in
      let ps = List.map ps ~f:adjust in
      let qs = List.map qs ~f:adjust in
      let ps = new_p :: ps in
      { ps; qs; vs }

  let add_data t ~data =
    List.fold data ~init:t ~f:add_point

  let create ~basis data =
    init basis
    |> add_data ~data

  let result { ps; vs; qs = _ } =
    let values = List.map vs ~f:snd in
    List.map2_exn ps values ~f:scale
    |> sum
end

let lagrange ~degree data =
  let basis = { Basis. degree; size = List.length data } in
  Lagrange.create ~basis data
  |> Lagrange.result

module Grid = struct
  type t = (float, float64_elt, c_layout) A2.t

  let create_empty (config : Config.t) =
    let (w, h) = Config.image_size config in
    A2.create Bigarray.float64 C_layout w h

  let create ~config ~eval =
    let open Int in
    let t = create_empty config in
    for i = 0 to A2.dim1 t - 1 do
      for j = 0 to A2.dim2 t - 1 do
        let x, y = Config.image_to_domain config (i, j) in
        A2.set t i j (eval x y)
      done
    done;
    t

  let zero ~config =
    let t = create_empty config in
    A2.fill t 0.;
    t
end

module Mono_cache = struct
  type t =
    { config : Config.t
    ; cache: Grid.t Mono.Table.t
    } [@@deriving fields]

  let create ~config : t =
    let cache = Mono.Table.create () in
    { config; cache }

  let find_or_add t mono =
    Hashtbl.find_or_add t.cache mono ~default:(fun () ->
      Grid.create ~config:t.config ~eval:(fun x y ->
        Mono.eval mono (Values.create [x; y])))
end

let eval_on_grid t ~cache =
  let open Int in
  let config = Mono_cache.config cache in
  let grid = Grid.zero ~config in
  List.iter t ~f:(fun (mono, weight) ->
    let mono_values = Mono_cache.find_or_add cache mono in
    for i = 0 to A2.dim1 grid - 1 do
      for j = 0 to A2.dim2 grid - 1 do
        let mono_value = A2.get mono_values i j in
        let grid_value = A2.get grid        i j in
        A2.set grid i j Float.(grid_value + mono_value * weight)
      done
    done);
  grid

module Lagrange_state = struct
  type t =
    { ps : string list
    ; qs : string list
    ; v : V.t
    } [@@deriving sexp]

  let _create ps qs v =
    let ps = List.map ps ~f:to_string in
    let qs = List.map qs ~f:to_string in
    { ps; qs; v }
end
