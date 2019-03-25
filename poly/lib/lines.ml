open Core
open Std_internal

let min_cover_size = 20

type t = Line.t list [@@deriving sexp]

let all_points config =
  let n_x, n_y = Config.grid_size config in
  List.cartesian_product
    (List.init n_x ~f:Fn.id)
    (List.init n_y ~f:Fn.id)

let all_diagonals config : t =
  let n_x, n_y = Config.grid_size config in
  let all_points = all_points config in
  let dirs = [ (1, 1); (1, -1) ] in
  List.init (n_x + n_y) ~f:(fun i ->
    List.concat_map dirs ~f:(fun dir ->
      [ (i, 0), dir
      ; (0, i), dir
      ]))
  |> List.concat
  |> List.filter ~f:(fun l ->
    let points = List.filter all_points ~f:(Line.mem l) in
    List.length points > 0)
  |> List.dedup_and_sort ~compare:Line.compare

let is_cover lines points =
  List.for_all points ~f:(fun p ->
    Set.exists lines ~f:(fun l -> Line.mem l p))

let random_diagonal_cover config : t =
  let all_points = all_points config in
  let rec loop cover to_remove =
    if Set.length cover <= min_cover_size then cover
    else match to_remove with
    | [] -> cover
    | l :: ls ->
      let cover' = Set.remove cover l in
      if is_cover cover' all_points
      then loop cover' ls
      else loop cover  ls
  in
  let lines = all_diagonals config |> List.permute in
  let cover = Line.Set.of_list lines in
  loop cover lines
  |> Set.to_list

let poly t =
  List.map t ~f:Line.poly
  |> P.product

let horizontal_lines config =
  let n_x, _ = Config.grid_size config in
  List.init n_x ~f:(fun i -> (0, i), (i, 0))

let vertical_lines config =
  let _, n_y = Config.grid_size config in
  List.init n_y ~f:(fun i -> (i, 0), (0, i))

let imo_vh config =
  intercalate
    (horizontal_lines config)
    (vertical_lines config)

let random_regular_line config =
  (imo_vh config)
  |> List.random_element_exn
