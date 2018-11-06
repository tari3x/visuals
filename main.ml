open Core
open Async
open Deferred.Let_syntax

module P = Polynomial
module A = Animation
module Config = A.Config

(* CR-someday: higher resolution when lines move fast. *)

(* TODO:
 * try plotting the surface
 * make it more generic than rectangular grids
 * understand what happens around the points such that zeroes persist.
 * make all lines move at once (direct interpolation)
 * the end of 4x5 is pretty abrupt
 * double log gives interesting colour effect
 * negative log gives interesting colour effect
 * how do I actually output two function tables and reuse them?
 * video quality
 * set samples vs set isosamples!! x vs y?
 * don't use deferred for maxima?
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

module type Ring = sig
  type t [@@deriving sexp, compare]

  include Commutative_group.S with type t := t

  val ( * ) : t -> t -> t
end

module Vector = struct
  module Make (R : Ring) = struct
    module T = struct
      type t = R.t * R.t [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make(T)

    let ( + ) (x1, y1) (x2, y2) : t =
      R.(x1 + x2, y1 + y2)

    let ( - ) (x1, y1) (x2, y2) : t =
      R.(x1 - x2, y1 - y2)

    let cross (x1, y1) (x2, y2) : R.t =
      R.(x1 * y2 - x2 * y1)
  end

  module Float = Make(Float)

  module Int = struct
    include Make(Int)

    let to_float (x, y) =
      (float x, float y)
  end
end

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

module Lines = struct
  let all_points { Config. n_x; n_y; _ } =
    List.cartesian_product
      (List.init n_x ~f:Fn.id)
      (List.init n_y ~f:Fn.id)

  let all_diagonals config =
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

  let is_cover lines points =
    List.exists points ~f:(fun p ->
      Set.for_all lines ~f:(fun l -> not (Line.mem l p)))

  let random_diagonal_cover config =
    let all_points = all_points config in
    let rec loop cover = function
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
    |> List.map ~f:Line.poly
    |> P.product

  let imo_lines { Config. n_x; n_y; _ } =
    let make_lines v n =
      List.init (n - 1) ~f:(fun i ->
        let i = i + 1 in
        P.(v - const (float i)))
      |> List.rev
    in
    intercalate
      (make_lines (P.var 1) n_x)
      (make_lines (P.var 2) n_y)

  let vertical_lines { Config. n_x; _ } =
    List.init n_x ~f:(fun i ->
      P.(var 1 - const (float i)))

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
    ; x_margin = 2
    ; y_margin = 2
    ; style = `heat
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
    | `both -> Lines.imo_lines config
    | `vertical -> Lines.vertical_lines config

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
      n_x = 5
    ; n_y = 4
    ; x_margin = 0
    ; y_margin = 0
    ; style = `heat
    ; show_dots = false
    }

  let imo_lines      = Lines.imo_lines config
  let vertical_lines = Lines.vertical_lines config

  let n_covers = 3

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
        (List.rev states1) @ states2 @ states
        |> return
    in
    loop (imo_lines, vertical_lines) covers

  let animate ~dir =
    let covers = List.init n_covers ~f:(fun _ ->
      Lines.random_diagonal_cover config)
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
    ; n_y = 6
    ; x_margin = 2
    ; y_margin = 2
    ; style = `heat
    ; show_dots = false
    }

  let n_lines = 10
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

  let initial_state () =
    List.init n_lines ~f:(fun _ -> random_line ())
    |> P.product
    |> A.State.create

  let animate ~dir =
    let lines = List.init n_steps ~f:(fun _ -> random_line ()) in
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

let command =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
     let dir = flag "-dir" (required file) ~doc:"" in
     fun () ->
       IMO_loop.animate ~dir
    ]

;;
Command.run command
