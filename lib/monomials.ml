open Std_internal

module L = Lagrange

let debug a = debug ~enabled:true a

let degree = 15

let config =
  Config.create
    ~grid_size:(10, 10)
    ~cbrange:(-15., 15.)
    ~image_width:896
    ()

let animate ~dir =
  P.Basis.mono ~degree
  |> List.map ~f:A.State.of_poly
  |> List.permute
  |> A.create ~config
  |> Render_camlimage.write ~dir ~degree
