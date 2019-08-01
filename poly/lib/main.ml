open Core
open Async

(* CR-someday: higher resolution when lines move fast. *)

(* vignette settings in kdenlive:
   smoothness 448
   radius 490
*)

(* TODO:
 * understand stutter better
 * understand what happens around the points such that zeroes persist.
 * the end of 4x5 is pretty abrupt
 * double log gives interesting colour effect
 * negative log gives interesting colour effect
 * set samples vs set isosamples!! x vs y?
 * don't use deferred for maxima?
 * Ruslan says 30fps
 * hack gnuplot to remove margins
*)

(* CL to GL
  https://software.intel.com/en-us/articles/opencl-and-opengl-interoperability-tutorial

  https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2D.xhtml
  https://www.khronos.org/registry/OpenCL/sdk/2.0/docs/man/xhtml/clCreateFromGLTexture.html
  https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/cl_image_format.html
  https://anteru.net/blog/2012/getting-started-with-opencl-part-3/
  https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/read_imagef2d.html
  https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/read_imagef2d.html
  https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/vectorDataTypes.html
*)

let draw =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
     let dir = flag "-dir" (required Filename.arg_type) ~doc:"" in
     fun () ->
       Grid.animate ~dir
          (*
       Lagrange.eval_test ();
       Async.return ()
          *)
    ]

let command =
  Command.group
    ~summary:""
    [ "draw", draw
    ; "window", Window.command
    ; "render-animation", Render_camlimage.command
    ]
