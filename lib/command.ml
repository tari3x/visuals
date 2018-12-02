open Core
open Async
open Deferred.Let_syntax

module P = Polynomial
module A = Animation
module Config = A.Config

(* CR-someday: higher resolution when lines move fast. *)

(* TODO:
 * understand what happens around the points such that zeroes persist.
 * the end of 4x5 is pretty abrupt
 * double log gives interesting colour effect
 * negative log gives interesting colour effect
 * set samples vs set isosamples!! x vs y?
 * don't use deferred for maxima?
 * Ruslan says 30fps
 * hack gnuplot to remove margins
*)

let () = Random.init 150

let fold_map_deferred xs ~init ~f =
  let rec loop y = function
    | [] -> return [y]
    | x :: xs ->
      let%bind y' = f y x in
      let%bind ys = loop y' xs in
      return (y :: ys)
  in
  loop init xs

let rec intercalate xs ys =
  match xs, ys with
  | [], ys -> ys
  | xs, [] -> xs
  | (x :: xs), (y :: ys) ->
    x :: y :: intercalate xs ys

module Point = Vector

module Line = struct
  module T = struct
    type t = Point.Int.t * Vector.Int.t [@@deriving sexp, compare]
  end

  include T

  let mem (p0, v) p =
    Vector.Int.(cross (p - p0) v) = 0

  let poly (p0, v) =
    let open Point.Int in
    P.zero_line_between_two_points
      (to_float p0)
      (to_float (p0 + v))

  include Comparable.Make(T)
end

(* Point range is [0; n - 1] *)
module Lines = struct
  type t = Line.t list [@@deriving sexp]

  let all_points { Config. n_x; n_y; _ } =
    List.cartesian_product
      (List.init n_x ~f:Fn.id)
      (List.init n_y ~f:Fn.id)

  let all_diagonals config : t =
    let { Config. n_x; n_y; _ } = config in
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
      if Set.length cover <= 20 then cover
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

  let horizontal_lines { Config. n_x; _ } =
    List.init n_x ~f:(fun i -> (0, i), (i, 0))

  let vertical_lines { Config. n_y; _ } =
    List.init n_y ~f:(fun i -> (i, 0), (0, i))

  let imo_vh config =
    intercalate
      (horizontal_lines config)
      (vertical_lines config)

  let random_regular_line config =
    (imo_vh config)
    |> List.random_element_exn
end

module IMO = struct
  let target_lines : [ `both | `vertical ] = `both

  (*
    let n_x = 3
    let n_y = 2
    let p =
    let l = P.zero_line_between_two_points in
    P.product
    [ l (0., 0.) (1., 1.)
    ; l (1., 0.) (2., 1.)
    ; l (2., 0.) (0., 1.)
    ]
  *)

  let config =
    { Animation.Config.
      n_x = 5
    ; n_y = 4
    ; left_margin = 2
    ; top_margin = 2
    ; right_margin = 5
    ; bottom_margin = 2
    ; style = `heat
    ; cbrange = (-10, 12)
    ; show_dots = false
    }

  let p =
    let l = P.zero_line_between_two_points in
    P.product
      [ l (0., 0.) (4., 2.)
      ; l (0., 1.) (2., 3.)
      ; l (0., 2.) (1., 1.)
      ; l (0., 3.) (4., 1.)
      ; l (1., 3.) (4., 0.)
      ; l (3., 3.) (2., 0.)
      ; l (4., 3.) (1., 0.)
      ; l (3., 0.) (4., 1.)
      ]

  (*&
    let x_margin = 5
    let y_margin = 5
    let n_x = 3
    let n_y = 3
    let p =
    let l = P.zero_line_between_two_points in
    List.init (2 * n_x - 2) ~f:(fun i ->
    let i = float (i + 1) in
    l (0., i) (i, 0.))
    |> P.product
  *)

  let lines_to_emerge =
    match target_lines with
    | `both -> Lines.imo_vh config
    | `vertical -> Lines.vertical_lines config

  let lines_to_emerge =
    List.map lines_to_emerge ~f:Line.poly

  let animate ~dir =
    (* CR-someday: you can skip the last iteration since last two lines fall in
       place together. *)
    let%bind states =
      fold_map_deferred lines_to_emerge
        ~init:(A.State.create p)
        ~f:A.State.emerge
    in
    A.write ~dir ~config states
end

module IMO_loop = struct
  let config =
    { Animation.Config.
      n_x = 7
    ; n_y = 5
    ; left_margin = 0
    ; top_margin = 0
    ; right_margin = 0
    ; bottom_margin = 0
    ; style = `heat
    ; cbrange = (-20, 25) (* (-10, 12) *)
    ; show_dots = false
    }

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
            ~init:(A.State.create c)
            ~f:step
        in
        let%bind states2 =
          fold_map_deferred emerge2
            ~init:(A.State.create c)
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
    A.write ~dir ~config states
end

(* CR: make sure lines don't repeat. Keep some straight lines to keep it
   interesting. *)
module Star = struct
  let config =
    { Animation.Config.
      n_x = 7
    ; n_y = 5
    ; left_margin = 0
    ; top_margin = 0
    ; right_margin = 0
    ; bottom_margin = 0
    ; style = `heat
    ; cbrange = (-20, 25)
    ; show_dots = false
    }

  let n_lines = 15
  let n_steps = 50

  let rec random_line () =
    let point () =
      let x = Random.int config.n_x in
      let y = Random.int config.n_y in
      (float x, float y)
    in
    let p1 = point () in
    let p2 = point () in
    if Point.Float.(p1 = p2)
    then random_line ()
    else P.zero_line_between_two_points p1 p2

      (*
  let initial_state () =
    List.init n_lines ~f:(fun _ -> random_line ())
    |> P.product
    |> A.State.create
      *)
  let initial_state () =
    Lines.random_diagonal_cover config
    |> Lines.poly
    |> A.State.create

  let animate ~dir =
    let lines = List.init n_steps ~f:(fun _ ->
      Lines.random_regular_line config |> Line.poly)
    in
    let%bind states =
      fold_map_deferred lines
        ~init:(initial_state ())
        ~f:(fun state l ->
          let%bind state = A.State.emerge state l in
          let state = A.State.collapse state in
          return state)
    in
    A.write ~dir ~config states
end

module Lagrange = struct
  let config =
    { Animation.Config.
      n_x = 4
    ; n_y = 4
    ; left_margin = 0
    ; top_margin = 0
    ; right_margin = 0
    ; bottom_margin = 0
    ; style = `heat
    ; cbrange = (-10, 12)
    ; show_dots = false
    }

  let set_value x =
    List.map ~f:(fun p -> (p, x))

  let grid =
    Lines.all_points config
    |> List.map ~f:Vector.Int.to_float
    |> set_value 0.

  let _pivots_2 =
    [
      [ (0.5, 0.5)
      ; (1.5, 1.3)
      ] |> set_value 1.
    ]

  let _pivots_3 =
    [
      [ (4., 3.)
      (* ; (1.4, 1.3)
      ; (3.4, 0.1)
      ; (2.4, 1.5)
      *)
      ] |> set_value 1.
    ]

  let _pivots_6 =
    [
      [ (1.4, 1.3)
      ; (3.4, 0.1)
      ; (2.4, 1.5)
      ] |> set_value 1.
    ]

  let animate ~dir =
    let points =
      List.init 10 ~f:(fun _ ->
        let x = Random.float 3. in
        let y = Random.float 3. in
        let value = Random.float 1. in
        ((x, y), value))
      |> List.return
    in
    let%bind states =
      Deferred.List.map points ~f:(fun points ->
       P.lagrange ~degree:3 points)
    in
    let states = List.map states ~f:A.State.create in
    A.write ~dir ~config ~interpolate:false states
end

let command =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
     let dir = flag "-dir" (required Filename.arg_type) ~doc:"" in
     fun () ->
       Lagrange.animate ~dir
    ]

