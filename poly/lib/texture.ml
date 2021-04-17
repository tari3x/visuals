open Core
open Tgl4
open Gl_common

type t =
  { id : int
  ; binding : int
  }

let last_binding = ref 0

let create ~w ~h =
  let binding = !last_binding in
  incr last_binding;
  let id = get_int (Gl.gen_textures 1) in
  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d id;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_image2d
    Gl.texture_2d
    0
    Gl.rgba32f
    w
    h
    0
    Gl.rgba
    Gl.float
    (`Offset 0);
  (* Because we're also using this tex as an image (in order to write to it),
     we bind it to an image unit as well *)
  Gl.bind_image_texture 0 id 0 false binding Gl.write_only Gl.rgba32f;
  check_errors "Gen texture";
  { id; binding }
;;
