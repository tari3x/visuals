open Std_internal

module L = Lagrange

let debug a = debug_s ~enabled:true a

let degree = 5

let config =
  Config.create
    ~grid_size:(4, 4)
    ~cbrange:(-0.5, 0.5)
    ~image_width:896
    ~rendering_degree:degree
    ()

let p =
  let l = P.zero_line_between_two_points in
  P.product
    [ l (1., 1.) (1., 2.)
    (* ; l (1., 1.) (2., 1.) *)
    ]

let show_dots =
  [ 0., 0.
  ; 1., 1.
  ; 2., 2.
  ]

let animate ~dir =
  debug [%message (p : P.t)];
  let state = A.State.of_poly p ~show_dots in
  A.create ~config [ state ]
  |> Render_camlimage.write ~dir

