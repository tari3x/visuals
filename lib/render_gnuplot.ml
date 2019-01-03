open Core
open Async

module A = Animation
module P = Polynomial
module E = Maxima.Expr
module V = Vector.Float

module Config = A.Config

module Writer = struct
  type t =
    { config : Config.t
    ; writer : Writer.t
    } [@@deriving fields]

  let add_dots w dots =
    fprintf w "unset object\n";
    List.iter dots ~f:(fun (x, y) ->
      fprintf w "set object circle front at first %f,%f \
                 radius char 0.5 fillstyle solid \
                 border lc rgb '#aa1100' lw 2\n" x y)

  let make_header { writer = w; config } =
    let { Config.
          n_x
        ; n_y
        ; left_margin
        ; top_margin
        ; right_margin
        ; bottom_margin
        ; _
        } = config
    in
    let%bind header = Reader.file_contents "header.gp" in
    fprintf w "%s" header;
    let x_start = -left_margin in
    let x_stop = n_x + right_margin in
    let y_start = -bottom_margin in
    let y_stop = n_y + top_margin in
    add_dots w config.show_dots;
    begin match config.style with
    | `zeroes ->
      fprintf w "set contour\n";
      fprintf w "set samples 100\n";
      fprintf w "set isosamples 100\n";
  (* interpolate doesn't work for some reason, empty plot
     set pm3dinterpolate 2, 2*)
    | `heat   ->
      fprintf w "set pm3d\n";
      fprintf w "set samples 500\n";
      fprintf w "set isosamples 500\n"
    end;
    fprintf w "set xrange [%d:%d]\n" x_start x_stop;
    fprintf w "set yrange [%d:%d]\n" y_start y_stop;
    let cb_x, cb_y = config.cbrange in
    fprintf w "set cbrange [%f:%f]\n" cb_x cb_y;
    (* # set terminal pngcairo size 2000, 1500 enhanced font 'Verdana,10' *)
    let x_units = n_x + left_margin + right_margin  - 1 in
    let y_units = n_y + top_margin  + bottom_margin - 1 in
    let width = 1600 in
    let height = width * y_units / x_units in
    (* pngcairo is 3x slower, although slightly better quality.*)
    fprintf w
      "set terminal png size %d, %d enhanced\n"
      width height;
    return ()

  let create ~dir ~config ~file_id =
    let filename = dir ^/ sprintf "make-frames%04d.gp" file_id in
    let%bind writer = Writer.open_file filename in
    let t = { writer; config } in
    let%bind () = make_header t in
    return t
end

let fun_id = ref 0

let make_fun_name () =
  incr fun_id;
  sprintf "f_gp%d" !fun_id

let write_defs w =
  List.iter ~f:(fun (f, p) ->
    fprintf w "%s(x, y) = %s\n" f (P.to_gnuplot p))

let frame = ref 0

let make_frame
    { Writer. writer = w; config }
    { A.State. p; ps; defs; dots } =
  incr frame;
  fprintf w "set output 'frame%06d.png'\n" !frame;
  let p = P.product (p :: ps) in
  let f = make_fun_name () in
  write_defs w defs;
  write_defs w [f, p];
  Writer.add_dots w dots;
  match config.style with
  | `zeroes ->
    fprintf w "splot(%s(x, y))\n" f
  | `heat ->
    fprintf w "splot(sgn(%s(x, y)) * log(abs(%s(x, y))))\n" f f

let print_monomials (w : Writer.t) =
  P.Mono.gnuplot_definitions ~degree:30
  |> List.iter ~f:(fprintf w.writer "%s\n")

let write ~dir ?batch_size {A. config; states } =
  let file_id = ref 0 in
  let new_writer () = Writer.create ~dir ~config ~file_id:!file_id in
  let%bind w = new_writer () in
  print_monomials w;
  let rec loop w i = function
    | [] -> return ()
    | s :: states ->
      make_frame w s;
      match batch_size with
      | Some size when i >= size ->
        incr file_id;
        let%bind w = new_writer () in
        loop w 0 states
      | _ ->  loop w (i + 1) states
  in
  loop w 0 states
