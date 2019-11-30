open Core
open Async
open Std_internal

module S = A.State

let _debug = debug_s ~enabled:true

let target_lines : [ `both | `vertical ] = `both

let n_x = 3
let n_y = 2

let config =
  Config.create
    ~grid_size:(n_x + 1, n_y + 1)
    (* CR: this is bigger than the screen? *)
    ~image_width:2048
    ~cbrange:(-1.5, 1.5)
    (* ~cbrange:(-10., 12.) *)
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
  let x = Float.of_int n_x in
  let y = Float.of_int n_y in
  let border_lines =
    [ (0., 0.), (1., 0.)
    ; (0., 0.), (0., 1.)
    ; (0., y), (1., 0.)
    ; (x, 0.), (0., 1.)
    ]
  in
  let lines =
    match target_lines with
    | `both -> Lines.imo_vh config
    | `vertical -> Lines.vertical_lines config
  in
  List.filter lines ~f:(fun l ->
    not (List.mem border_lines l ~equal:Line.equal))

let show_dots =
  List.cartesian_product
    [0.; 1.; 2.; 3.]
    [0.; 1.; 2.]

let next (s : S.t)  (l : Line.t) =
  let s = S.map s ~f:(Line.deform_to_zero_preserving_boundary l config) in
  S.emerge s l

let animate ~dir =
  let init = S.of_poly p ~show_dots in
  let%bind states =
    List.scan_deferred
      lines_to_emerge
      ~init
      ~f:next
  in
  List.map (init :: states) ~f:S.collapse
  |> S.interpolate
  |> S.suspend_end
  |> A.create ~config
  |> Render_camlimage.write ~dir
