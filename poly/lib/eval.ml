open Core
open Common

module P = Polynomial

let debug a = debug_s ~enabled:false a

open Float_array

module Ctx = struct
  open Opencl

  module Cl_mem = struct
    type t = (float, float64_elt) Cl.Mem.t
  end

  module Cl_mem_int = struct
    type t = (int, int_elt) Cl.Mem.t
  end

  type t =
    { config : Config.t
    ; work_group_size : int
    ; context : Cl.Context.t
    ; monos : P.t list
    ; monos_g : Cl_mem.t
    ; result_size : int
    ; result_h : A2_int.t
    ; result_g : Cl_mem_int.t
    ; coeffs_h : A1.t
    ; coeffs_g : Cl_mem.t
    ; queue : Cl.Command_queue.t
    ; program : Cl.Program.t
    ; kernel : Cl.Kernel.t
    } [@@deriving fields]

  let build program device =
    try Cl.build_program program [device] "";
    with Cl.Cl_error cl_error ->
      begin
        Printf.eprintf
          "error building program %s.\n%!"
          (Cl.Cl_error.to_string cl_error);
        let build_log =
          Cl.get_program_build_info program device
            Cl.Program_build_info.build_log
        in
        let build_status =
          Cl.get_program_build_info program device
            Cl.Program_build_info.build_status
        in
        Printf.eprintf "%s %s\n%!" build_log
          (Cl.Build_status.to_string build_status)
      end

  let create ~config : t =
    let open Int in
    let degree = Config.rendering_degree_exn config in
    let (min_value, max_value) = Config.cbrange config in
    let num_monos = P.Basis.mono ~degree |> List.length in
    let platforms = Cl.get_platform_ids () in
    let platform = List.hd_exn platforms in

    let devices = Cl.get_device_ids platform [Cl.Device_type.ALL] in
    let device = List.hd_exn devices in

    let context = Cl.create_context [] [device] in
    let queue = Cl.create_command_queue context device [] in
    (* 1e-7: Offset it a bit so we are less likely to hit an exact zero in
       log *)
    let code = sprintf
      {|
          __kernel void poly_eval_kernel (
            __global const double* monos,
            __global const double* coeffs,
            __global long* result,
            const int result_size)
            {
              int i = get_global_id(0);

              double min_value = %f;
              double max_value = %f;
              double value_range = %f;
              int num_monos = %d;
              int last_color = %d;
              double r = 0;

              for(int k = 0; k < num_monos; k++)
                r += coeffs[k] * monos[k * result_size + i];
                // done;

              r = r + 1e-7;

              if (r > 0) // then
                r = log(r);
              if (r < 0) // then
                r = -log(-r);

              r = max(r, min_value);
              r = min(r, max_value);

              r = (r - min_value) / value_range;

              // printf ("%%f\n", r);
              result[i] = r * last_color;
            }
          |}
      min_value
      max_value
      (max_value -. min_value)
      num_monos
      (Palette.num_colors - 1)
    in
    let program = Cl.create_program_with_source context [code] in
    build program device;
    let kernel = Cl.create_kernel program "poly_eval_kernel" in
    let x_size, y_size = Config.image_size config in
    let result_size = x_size * y_size in
    (* CR-someday: ask the GPU. *)
    let work_group_size = 256 in
    if result_size % work_group_size <> 0
    then failwith "result size must be a multiple of max_work_group_size";
    let monos_h  = A3.create num_monos x_size y_size in
    let result_h = A2_int.create x_size y_size in
    let coeffs_h = A1.create num_monos in
    let monos = P.first_monomials num_monos |> List.sort ~compare:P.compare in
    let monos_size =
      A3.dim1 monos_h * A3.dim2 monos_h * A3.dim3 monos_h
    in
    debug [%message (num_monos : int)];
    debug [%message (monos_size : int)];
    let monos_g =
      Cl.create_buffer context
        Cl.Mem_flags.([READ_ONLY])
        (Cl.Buffer_contents.SIZE (Bigarray.float64, monos_size))
    in
    let result_g =
      Cl.create_buffer context
        Cl.Mem_flags.([WRITE_ONLY])
        (Cl.Buffer_contents.SIZE (Bigarray.int,
                                  A2.dim1 result_h * A2.dim2 result_h))
    in
    let coeffs_g =
      Cl.create_buffer context
        Cl.Mem_flags.([READ_ONLY])
        (Cl.Buffer_contents.SIZE (Bigarray.float64, A1.dim coeffs_h))
    in
    Cl.set_kernel_arg kernel 0 (MEM monos_g);
    Cl.set_kernel_arg kernel 1 (MEM coeffs_g);
    Cl.set_kernel_arg kernel 2 (MEM result_g);
    Cl.set_kernel_arg kernel 3 (SCALAR (int32, Int32.of_int_exn result_size));
    List.iteri monos ~f:(fun k mono ->
      let eval i j =
        let x, y = Config.image_to_domain config (i, j) in
        P.eval mono [x; y]
      in
      for i = 0 to x_size - 1 do
        for j = 0 to y_size - 1 do
          monos_h.{k, i, j} <- eval i j
        done
      done);
    let event =
      Cl.enqueue_write_buffer queue monos_g true (genarray_of_array3 monos_h) []
    in
    Cl.release_event event;
    { config
    ; work_group_size
    ; context
    ; monos
    ; monos_g
    ; result_size
    ; result_h
    ; result_g
    ; coeffs_h
    ; coeffs_g
    ; queue
    ; program
    ; kernel
    }

  let create ~config =
    try create ~config
    with Cl.Cl_error cl_error ->
      failwithf "error %s.\n%!" (Cl.Cl_error.to_string cl_error) ()

  let eval t p =
    let coeffs = P.monomials p |> P.Map.of_alist_exn in
    List.iteri t.monos ~f:(fun i m ->
      let c =
        match Map.find coeffs m with
        | Some c -> c
        | None -> 0.
      in
      t.coeffs_h.{i} <- c);
    let event =
      Cl.enqueue_write_buffer t.queue t.coeffs_g
        true
        (genarray_of_array1 t.coeffs_h) []
    in
    Cl.release_event event;
    let event =
      Cl.enqueue_nd_range_kernel
        t.queue
        t.kernel
        None
        [t.result_size]
        (Some [t.work_group_size])
        []
    in
    Cl.wait_for_events [event];
    Cl.release_event event;
    let event =
      Cl.enqueue_read_buffer t.queue t.result_g true
        (genarray_of_array2 t.result_h) []
    in
    Cl.wait_for_events [event];
    Cl.release_event event;
    t.result_h

  let eval t p =
    try eval t p
    with Cl.Cl_error cl_error ->
      failwithf "error %s.\n%!" (Cl.Cl_error.to_string cl_error) ()

  let release
      { config = _
      ; work_group_size = _
      ; context
      ; monos = _
      ; monos_g
      ; result_size = _
      ; result_h = _
      ; result_g
      ; coeffs_h = _
      ; coeffs_g
      ; queue
      ; program
      ; kernel
      } =
    Cl.release_kernel kernel;
    Cl.release_program program;
    Cl.release_command_queue queue;
    Cl.release_context context;
    Cl.release_mem_object result_g;
    Cl.release_mem_object monos_g;
    Cl.release_mem_object coeffs_g
end

let values = Ctx.eval
