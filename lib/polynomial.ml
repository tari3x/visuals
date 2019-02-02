open Core
open Float_array

open Common

module E = Maxima.Expr
module V = Vector.Float
module Matrix = Maxima.Matrix

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
module Args = struct
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

  let eval (t : t) (args : Args.t) =
    List.map t ~f:(fun (v, n) ->
      Float.int_pow (Args.find_exn args v) n)
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

module Monos = struct
  type t = (Mono.t * float) list
end

type t = float Mono.Map.t

let zero : t = Mono.Map.empty

let const x : t =
  Mono.Map.singleton Mono.one x

let mono m : t =
  Mono.Map.singleton m 1.

let var n : t =
  mono (Mono.var n)

let call name : t =
  mono (Mono.call name)

let verbatim s : t =
  mono (Mono.verbatim s)

let group_sum (monos : Monos.t) : t =
  Mono.Map.of_alist_multi monos
  |> Map.filter_map ~f:(fun coeffs ->
    let c = Float.sum coeffs in
    if Float.(c = 0.) then None
    else Some c)

(* CR-someday: [Map.merge]. *)
let ( + ) (t1 : t) (t2 : t) : t =
  group_sum (Map.to_alist t1 @ Map.to_alist t2)

let ( * ) (t1 : t) (t2 : t) : t =
  List.cartesian_product (Map.to_alist t1) (Map.to_alist t2)
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
  Map.to_alist t
  |> List.map ~f:(fun (m, c) ->
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

let eval (t : t) (args : float list) =
  let args = Args.create args in
  Map.to_alist t
  |> List.map ~f:(fun (m, c) ->
    Float.(c * Mono.eval m args))
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

module Eval_ctx = struct
  open Opencl

  module GPU_mem = struct
    type t = (float, float64_elt) Cl.Mem.t
  end

  type t =
    { config : Config.t
    ; context : Cl.Context.t
    ; monos_g : GPU_mem.t
    ; result_h : A2.t
    ; result_g : GPU_mem.t
    ; coeffs_h : A1.t
    ; coeffs_g : GPU_mem.t
    ; queue : Cl.Command_queue.t
    ; kernel : Cl.Kernel.t
    } [@@deriving fields]

  let build program device =
    try Cl.build_program program [device] "";
    with _ ->
      begin
        let build_log =
          Cl.get_program_build_info program device
            Cl.Program_build_info.build_log
        in
        let build_status =
          Cl.get_program_build_info program device
            Cl.Program_build_info.build_status
        in
        Printf.printf "%s %s\n%!" build_log
          (Cl.Build_status.to_string build_status)
      end

  let create ~config ~num_monos : t =
    let open Int in
    let platforms = Cl.get_platform_ids () in
    let platform = List.hd_exn platforms in

    let devices = Cl.get_device_ids platform [Cl.Device_type.ALL] in
    let device = List.hd_exn devices in

    let context = Cl.create_context [] [device] in
    let queue = Cl.create_command_queue context device [] in
    let program = Cl.create_program_with_source context ["
      __kernel void poly_eval_kernel (
        __global const double* monos,
        __global const double* coeffs,
        __global double* result,
        const int result_size,
        const int num_monos)
      {
        int i = get_global_id(0);

        result[i] = 0;

        for(int k = 0; k < num_monos; k++){
          int monos_idx = k * result_size + i;
          result[i] += monos[monos_idx] * coeffs[k];
        }
      }"]
    in
    build program device;
    let kernel = Cl.create_kernel program "poly_eval_kernel" in
    let x_size, y_size = Config.image_size config in
    let result_size = x_size * y_size in
    (* CR: ask the GPU. *)
    let work_group_size = 128 in
    if result_size % work_group_size <> 0
    then failwith "result size must be a multiple of max_work_group_size";
    let monos_h  = A3.create num_monos x_size y_size in
    let result_h = A2.create x_size y_size in
    let coeffs_h = A1.create num_monos in
    let monos = Mono.first num_monos |> List.sort ~compare:Mono.compare in
    List.iteri monos ~f:(fun k mono ->
      let eval x y = Mono.eval mono (Args.create [x; y]) in
      for i = 0 to x_size - 1 do
        for j = 0 to y_size - 1 do
          let x, y = Config.image_to_domain config (i, j) in
          monos_h.{k, i, j} <- eval x y
        done
      done);
    let monos_g =
      Cl.create_buffer context
        Cl.Mem_flags.([READ_ONLY])
        (Cl.Buffer_contents.SIZE (Bigarray.float64,
                                  Array3.dim1 monos_h
                                  * Array3.dim2 monos_h
                                  * Array3.dim3 monos_h))
    in
    let result_g =
      Cl.create_buffer context
        Cl.Mem_flags.([WRITE_ONLY])
        (Cl.Buffer_contents.SIZE (Bigarray.float64,
                                  Array2.dim1 result_h * Array2.dim2 result_h))
    in
    let coeffs_g =
      Cl.create_buffer context
        Cl.Mem_flags.([READ_ONLY])
        (Cl.Buffer_contents.SIZE (Bigarray.float64, Array1.dim coeffs_h))
    in
    Cl.set_kernel_arg kernel 0 (Cl.Arg_value.MEM monos_g);
    Cl.set_kernel_arg kernel 1 (Cl.Arg_value.MEM coeffs_g);
    Cl.set_kernel_arg kernel 2 (Cl.Arg_value.MEM result_g);
    Cl.set_kernel_arg kernel 3 (Cl.Arg_value.SCALAR
                                  (int32, Int32.of_int_exn result_size));
    Cl.set_kernel_arg kernel 4 (Cl.Arg_value.SCALAR
                                  (int32, Int32.of_int_exn num_monos));
    let event =
      Cl.enqueue_write_buffer queue monos_g true (genarray_of_array3 monos_h) []
    in
    Cl.release_event event;
    { config
    ; context
    ; monos_g
    ; result_h
    ; result_g
    ; coeffs_h
    ; coeffs_g
    ; queue
    ; kernel
    }

  let eval t p =
    for k = 0 to num_monos - 1 do
      coeffs_h.{k} <- float (n + k + 1);
    done;
    let event =
      Cl.enqueue_write_buffer queue coeffs_g true (genarray_of_array1 coeffs_h) []
    in
    Cl.release_event event;
    let event =
      Cl.enqueue_nd_range_kernel
        queue
        kernel
        None
        [result_size]
        (Some [work_group_size])
        []
    in
    Cl.release_event event;
    let event =
      Cl.enqueue_read_buffer queue result_g true (genarray_of_array2 result_h) []
    in
    Cl.release_event event;
end

let eval_grid cache t =
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
