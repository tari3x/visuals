open Core
open Async
open Std_internal

let config =
  Config.create
    ~grid_size:(7, 5)
    ~cbrange:(-20., 25.) (* (-10, 12) *)
    ()

let imo_lines      = Lines.imo_vh config         |> List.map ~f:Line.poly
let vertical_lines = Lines.vertical_lines config |> List.map ~f:Line.poly

let n_covers = 3

  (* CR: you are reversing the whole list. *)
let states covers =
  let step state line =
    let%bind state = A.State.emerge state line in
    let state = A.State.collapse state in
    return state
  in
  let rec loop (emerge1, emerge2) covers =
    match covers with
    | [] -> return []
    | c :: cs ->
      let%bind states1 =
        fold_map_deferred emerge1
          ~init:(A.State.of_poly c)
          ~f:step
      in
      let%bind states2 =
        fold_map_deferred emerge2
          ~init:(A.State.of_poly c)
          ~f:step
      in
      let%bind states = loop (emerge2, emerge1) cs in
      (List.rev states1) @ (List.tl_exn states2) @ states
      |> return
  in
  loop (imo_lines, vertical_lines) covers

let animate ~dir =
  let covers = List.init n_covers ~f:(fun _ ->
    let c = Lines.random_diagonal_cover config in
    printf !"%{sexp:Lines.t}" c;
    Lines.poly c)
  in
  let%bind states = states covers in
  A.State.interpolate states
  |> A.create ~config
  |> Render_gnuplot.write ~dir
