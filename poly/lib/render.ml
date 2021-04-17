open Core
open Tgl4
open Bigarray
open Gl_common

type t = { prog : int }

let create (texture : Texture.t) =
  let prog = Gl.create_program () in
  let vp = Gl.create_shader Gl.vertex_shader in
  let fp = Gl.create_shader Gl.fragment_shader in
  let vp_src =
    {|#version 430
     in vec2 pos;
    out vec2 texCoord;
    void main() {
      texCoord = pos*0.5f + 0.5f;
      gl_Position = vec4(pos.x, pos.y, 0.0, 1.0);
    }
    |}
  in
  let fp_src =
    {|#version 430
        uniform sampler2D srcTex;
     in vec2 texCoord;
    out vec4 color;
    void main() {
      color = texture(srcTex, texCoord);
    }
    |}
  in
  Gl.shader_source vp vp_src;
  Gl.shader_source fp fp_src;
  Gl.compile_shader vp;
  let rvalue = get_int (Gl.get_shaderiv vp Gl.compile_status) in
  if rvalue = 0
  then (
    get_shader_compile_log ~shader:vp;
    failwithf "_error in compiling vp\n" ());
  Gl.attach_shader prog vp;
  Gl.compile_shader fp;
  let rvalue = get_int (Gl.get_shaderiv fp Gl.compile_status) in
  if rvalue = 0 then failwithf "_error in compiling fp\n" ();
  Gl.attach_shader prog fp;
  Gl.link_program prog;
  let rvalue = get_int (Gl.get_programiv prog Gl.link_status) in
  if rvalue = 0 then failwithf "_error in linking sp\n" ();
  Gl.use_program prog;
  Gl.bind_frag_data_location prog 0 "color";
  Gl.uniform1i (Gl.get_uniform_location prog "srcTex") texture.binding;
  let vert_array = get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vert_array;
  let pos_buf = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer pos_buf;
  let data =
    [| -1.0; -1.0; -1.0; 1.0; 1.0; -1.0; 1.0; 1.0 |]
    |> Array1.of_array Float64 C_layout
  in
  Gl.buffer_data
    Gl.array_buffer
    (Array1.size_in_bytes data)
    (Some data)
    Gl.stream_draw;
  let pos_ptr = Gl.get_attrib_location prog "pos" in
  Gl.vertex_attrib_pointer pos_ptr 2 Gl.double false 0 (`Offset 0);
  Gl.enable_vertex_attrib_array pos_ptr;
  check_errors "_render shaders";
  { prog }
;;
