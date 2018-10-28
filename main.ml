open Core
open Async
open Deferred.Let_syntax

module P = Polynomial
module D = P.Division_result

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
*)

let target_lines : [ `both | `vertical ] = `both
let style : [ `zeroes | `heat ] = `heat
let show_dots = false

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

let n_x = 5
let n_y = 4
let x_margin = 2
let y_margin = 2
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

let frame = ref 0

let make_frame w p =
  incr frame;
  fprintf w "set output 'frame%04d.png'\n" !frame;
  fprintf w "f(x, y) = %s\n" (P.to_gnuplot p);
  match style with
  | `zeroes ->
    fprintf w "splot(f(x, y))\n"
  | `heat ->
    fprintf w "splot(sgn(f(x, y)) * log(abs(f(x, y))))\n"

let interpolate w p1 p2 =
  let num_steps = 100 in
  for n = 0 to num_steps do
    let alpha = float n /. float num_steps in
    make_frame w P.(scale p1 (1. -. alpha) + scale p2 alpha)
  done

let make_header w =
  let%bind header = Reader.file_contents "header.gp" in
  fprintf w "%s" header;
  let x_start = -x_margin in
  let x_stop = n_x + x_margin + 3 in
  let y_start = -y_margin in
  let y_stop = n_y + y_margin in
  if show_dots then begin
    for i = 0 to n_x - 1 do
      for j = 0 to n_y - 1 do
        fprintf w "set object circle at first %d,%d \
                 radius char 0.5 fillstyle empty \
                 border lc rgb '#aa1100' lw 2\n" i j;
      done
    done
  end;
  begin match style with
  | `zeroes ->
    fprintf w "set contour\n";
    fprintf w "set isosamples 100, 100\n"
  (* interpolate doesn't work for some reason, empty plot
     set pm3dinterpolate 2, 2*)
  | `heat   ->
    fprintf w "set pm3d\n";
    fprintf w "set isosamples 500, 500\n"
  end;
  fprintf w "set xrange [%d:%d]\n" x_start x_stop;
  fprintf w "set yrange [%d:%d]\n" y_start y_stop;
  (* # set terminal pngcairo size 2000, 1500 enhanced font 'Verdana,10' *)
  let x_units = n_x + 2 * x_margin - 1 in
  let y_units = n_y + 2 * y_margin - 1 in
  let width = 1600 in
  let height = width * y_units / x_units in
  fprintf w
    "set terminal pngcairo size %d, %d enhanced font 'Verdana,10'\n"
    width height;
  return ()

let new_make_table_name =
  let counter = ref 0 in
  fun () ->
    incr counter;
    sprintf "make-table-%d.gp"

let make_values_table p1 p2 =
  let table_name = new_make_table_name () in
  let%bind table_w = Writer.open_file (dir ^/ table_name) in
  let%bind () = make_table_header t_w in
  fprintf t_w "p1(x, y) = %s" (Poly.to_gnuplot p1);
  fprintf t_w "p2(x, y) = %s" (Poly.to_gnuplot p2);
  fprintf t_w "do for [i = 0 : %d]{" isosamples;
  fprintf t_w "x = %f + (1.0 * i) * %f / %d" x_start x_width isosamples;
  fprintf t_w "do for [j = 0 : %d]{" isosamples;
  fprintf t_w "y = %f + (1.0 * i) * %f / %d" y_start y_width isosamples;
  fprintf t_w "print x, y, p1(x, y), p2(x, y)";
  fprintf t_w "}";
  fprintf t_w "}";
  return table_name

let interpolate w p1 p2 =
  let%bind table_name = make_values_table w p1 p2 in
  let num_steps = 100 in
  for n = 0 to num_steps do
    let alpha = float n /. float num_steps in
    make_frame w P.(scale p1 (1. -. alpha) + scale p2 alpha)
  done


let rec intercalate xs ys =
  match xs, ys with
  | [], ys -> ys
  | xs, [] -> xs
  | (x :: xs), (y :: ys) ->
    x :: y :: intercalate xs ys

let imo_lines n_x n_y =
  let make_lines v n =
    List.init (n - 1) ~f:(fun i ->
      let i = i + 1 in
      P.(v - const (float i)))
    |> List.rev
  in
  intercalate
    (make_lines (P.var 1) n_x)
    (make_lines (P.var 2) n_y)

let vertical_lines n_x =
  List.init n_x ~f:(fun i ->
    P.(var 1 - const (float i)))

let write_frames w p =
  (* CR-someday: you can skip the last iteration since last two lines fall in
     place together. *)
  let lines =
    match target_lines with
    | `both -> imo_lines n_x n_y
    | `vertical -> vertical_lines n_x
  in
  interpolate w p (P.product lines)
  (*
  let rec loop p i ~lines_p ~lines =
    match lines with
    | [] -> return ()
    | l :: lines ->
      let%bind { D. q; r } = P.divide p l in
      interpolate w P.(lines_p * p) P.(lines_p * (p - r));
      let lines_p = P.(l * lines_p) in
      loop q (i + 1) ~lines_p ~lines
  in
  loop p 0 ~lines_p:(P.const 1.) ~lines
  *)

let main dir p =
  let%bind w = Writer.open_file (dir ^/ "make-frames.gp") in
  let%bind () = make_header w in
  write_frames w p;
  return ()

let command =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
     let dir = flag "-dir" (required file) ~doc:"" in
     fun () ->
       main dir p
    ]

;;
Command.run command
