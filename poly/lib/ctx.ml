open Core
open Tgl4
open Gl_common
open Float_array
module P = Polynomial

let debug = Core.print_s

module Win = Sdl_window

module Compute = struct
  let local_size_x = 16
  let local_size_y = 16

  type t =
    { config : Config.t
    ; prog : int
    ; monos : P.t list
    ; monos_g : Ssbo.Scalar.t
    ; coeffs_h : A1.t
    ; coeffs_g : Ssbo.Scalar.t
    ; colors_g : Ssbo.Color.t
    }

  let create_colors_g () =
    Ssbo.Color.create_empty ~length:Palette.num_colors
  ;;

  (* CR-someday: map the buffer straight on the GPU. *)
  let create_monos_g config =
    let w, h = Config.image_size config in
    let degree = Config.rendering_degree_exn config in
    let num_monos = P.Basis.mono ~degree |> List.length in
    let data = Array3.create Float64 C_layout num_monos w h in
    let monos =
      P.first_monomials num_monos |> List.sort ~compare:P.compare
    in
    List.iteri monos ~f:(fun k mono ->
        let eval i j =
          let x, y = Config.image_to_domain config (i, j) in
          P.eval mono [ x; y ]
        in
        for i = 0 to w - 1 do
          for j = 0 to h - 1 do
            data.{k, i, j} <- eval i j
          done
        done);
    Ssbo.Scalar.create3 data
  ;;

  let create_coeffs_h ~num_monos = A1.create num_monos
  let create_coeffs_g ~coeffs_h = Ssbo.Scalar.create1 coeffs_h

  let create (config : Config.t) (texture : Texture.t) =
    let min_value, max_value = Config.cbrange config in
    let degree = Config.rendering_degree_exn config in
    let num_monos = P.Basis.mono ~degree |> List.length in
    let monos =
      P.first_monomials num_monos |> List.sort ~compare:P.compare
    in
    let monos_g = create_monos_g config in
    let coeffs_h = create_coeffs_h ~num_monos in
    let coeffs_g = create_coeffs_g ~coeffs_h in
    let colors_g = create_colors_g () in
    let prog = Gl.create_program () in
    let cs = Gl.create_shader Gl.compute_shader in
    (* In order to write to a texture, we have to introduce it as image2D.
     local_size_x/y/z layout variables define the work group size.
     Gl._GlobalInvocationID is a uvec3 variable giving the global ID of the thread,
     Gl._LocalInvocationID is the local index within the work group, and
       Gl._WorkGroupID is the work group's index
    *)
    let code =
      [%string
        {|#version 430
%{Ssbo.Scalar.glsl_declaration monos_g ~name:"monos"}
%{Ssbo.Scalar.glsl_declaration coeffs_g ~name:"coeffs"}
%{Ssbo.Color.glsl_declaration colors_g ~name:"colors"}
uniform writeonly image2D destTex;
layout (local_size_x = %{local_size_x#Int}, local_size_y = %{local_size_y#Int}) in;
          void main() {
            ivec2 pos = ivec2(gl_GlobalInvocationID.xy);

            double min_value = %{min_value#Float};
            double max_value = %{max_value#Float};
            double value_range = %{(max_value -. min_value)#Float};
            int num_monos = monos.length();
            int num_colors = %{Palette.num_colors#Int};
            double r = 0;

            for(int k = 0; k < num_monos; k++)
              r += coeffs[k] * monos[k][pos.x][pos.y];
              // done;

            r +=+ 1e-7;

            // there's no double log, wut?
            if (r > 0) // then
              r = log(float(r));
            if (r < 0) // then
              r = -log(float(-r));

            r = max(r, min_value);
            r = min(r, max_value);

            r = (r - min_value) / value_range;

            // no printf, wut?
            vec4 color = colors[int(r * float(num_colors - 1))];
            imageStore(destTex, pos, color);
          }
      |}]
    in
    debug [%message (code : string)];
    Gl.shader_source cs code;
    Gl.compile_shader cs;
    let rvalue = create_uint32_array 1 in
    Gl.get_shaderiv cs Gl.compile_status rvalue;
    if Int32.(Array1.get rvalue 0 = 0l)
    then (
      Core.eprintf "_error in compiling the compute shader\n";
      get_shader_compile_log ~shader:cs;
      failwith "exit");
    Gl.attach_shader prog cs;
    Gl.link_program prog;
    Gl.get_programiv prog Gl.link_status rvalue;
    if Int32.(Array1.get rvalue 0 = 0l)
    then (
      Core.eprintf "_error in linking compute shader program\n";
      get_program_info_log ~prog;
      failwith "exit");
    Gl.use_program prog;
    Gl.uniform1i (Gl.get_uniform_location prog "destTex") texture.binding;
    check_errors "_compute shader";
    { config; prog; monos; monos_g; coeffs_h; coeffs_g; colors_g }
  ;;

  let update_coeffs t p =
    let coeffs = P.monomials p |> P.Map.of_alist_exn in
    List.iteri t.monos ~f:(fun i m ->
        let c =
          match Map.find coeffs m with
          | Some c -> c
          | None -> 0.
        in
        t.coeffs_h.{i} <- c);
    Ssbo.Scalar.update1 t.coeffs_g t.coeffs_h
  ;;

  let dispatch t (win : Win.t) p =
    Gl.use_program t.prog;
    update_coeffs t p;
    Gl.dispatch_compute (win.w / local_size_x) (win.h / local_size_y) 1;
    check_errors "_dispatch compute shader"
  ;;

  let update_colors t (palette : Palette.t) =
    Ssbo.Color.update t.colors_g palette
  ;;
end

type t =
  { win : Win.t
  ; render : Render.t
  ; compute : Compute.t
  }

let create config =
  let w, h = Config.image_size config in
  (* CR-someday: must create [Win] before everything else *)
  let win = Win.create ~w ~h |> ok_exn in
  let texture = Texture.create ~w ~h in
  let render = Render.create texture in
  let compute = Compute.create config texture in
  Compute.update_colors compute Palette.rgb;
  { win; render; compute }
;;

let draw t p =
  Compute.dispatch t.compute t.win p;
  (* Gl.clear Gl.color_buffer_bit; *)
  Gl.use_program t.render.prog;
  Gl.draw_arrays Gl.triangle_strip 0 4;
  (* Sdl.gl_swap_window window; *)
  Win.swap_buffers t.win;
  check_errors "draw screen"
;;

let update_colors t palette = Compute.update_colors t.compute palette
let poll_event t = Win.poll_event t.win
