open Core
open Async
open Std_internal

let _debug = debug_s ~enabled:true

let target_lines : [ `both | `vertical ] = `both

let n_x = 3
let n_y = 2

let config =
  Config.create
    ~grid_size:(n_x + 1, n_y + 1)
    (* CR: this is bigger than the screen? *)
    ~image_width:2048
    ~cbrange:(-10., 12.)
    ~rendering_degree:12
    ()

let p =
  let l = P.zero_line_between_two_points in
  P.product
    [ l (0., 0.) (1., 2.)
    ; l (0., 1.) (2., 2.)
    ; l (0., 2.) (3., 0.)
    ; l (3., 2.) (2., 0.)
    ; l (3., 1.) (1., 0.)
    ]

let lines_to_emerge =
  let border_lines =
    [ (0, 0), (1, 0)
    ; (0, 0), (0, 1)
    ; (0, n_y), (1, 0)
    ; (n_x, 0), (0, 1)
    ]
  in
  let lines =
    match target_lines with
    | `both -> Lines.imo_vh config
    | `vertical -> Lines.vertical_lines config
  in
  List.filter lines ~f:(fun l ->
    not (List.mem border_lines l ~equal:Line.equal))

let animate ~dir =
  let init = A.State.of_poly p in
  let%bind states =
    List.scan_deferred
      lines_to_emerge
      ~init
      ~f:A.State.emerge
  in
  List.map (init :: states) ~f:A.State.collapse
  |> A.State.interpolate
  |> A.State.suspend_end
  |> A.create ~config
  |> Render_camlimage.write ~dir
