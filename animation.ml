open Core
open Async

module P = Polynomial
module D = P.Division_result

module Config = struct
  type t =
    { n_x : int
    ; n_y : int
    ; left_margin : int
    ; top_margin : int
    ; right_margin : int
    ; bottom_margin : int
    ; style : [ `zeroes | `heat ]
    ; cbrange : int * int
    ; show_dots : bool
    }
end

module Writer = struct
  type t =
    { config : Config.t
    ; writer : Writer.t
    } [@@deriving fields]

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
    if config.show_dots then begin
      for i = 0 to n_x - 1 do
        for j = 0 to n_y - 1 do
          fprintf w "set object circle at first %d,%d \
                 radius char 0.5 fillstyle empty \
                 border lc rgb '#aa1100' lw 2\n" i j;
        done
      done
    end;
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
    fprintf w "set cbrange [%d:%d]\n" cb_x cb_y;
  (* # set terminal pngcairo size 2000, 1500 enhanced font 'Verdana,10' *)
    let x_units = n_x + left_margin + right_margin  - 1 in
    let y_units = n_y + top_margin  + bottom_margin - 1 in
    let width = 1600 in
    let height = width * y_units / x_units in
  (* pngcairo is 3x slower, although slightly better quality.*)
    fprintf w
      "set terminal png size %d, %d enhanced font 'Verdana,10'\n"
      width height;
    return ()

  let create ~dir ~config =
    let%bind writer = Writer.open_file (dir ^/ "make-frames.gp") in
    let t = { writer; config } in
    let%bind () = make_header t in
    return t
end

let fun_id = ref 0

let define (w : Writer.t) p =
  incr fun_id;
  let f = sprintf "f%d" !fun_id in
  fprintf w.writer "%s(x, y) = %s\n" f (P.to_gnuplot p);
  f

let frame = ref 0

let make_frame { Writer. writer = w; config } f =
  incr frame;
  fprintf w "set output 'frame%04d.png'\n" !frame;
  let f = P.to_gnuplot f in
  match config.style with
  | `zeroes ->
    fprintf w "splot(%s)\n" f
  | `heat ->
    fprintf w "splot(sgn(%s) * log(abs(%s)))\n" f f

module State = struct
  (* animated and static parts *)
  type t = P.t * P.t list

  let create p =
    (p, [])

  let product (p, ps) =
    P.product (p :: ps)

  let collapse t =
    (product t, [])

  let interpolate w t1 t2 =
    let num_steps = 100 in
    let p1 = product t1 in
    let p2 = product t2 in
    (* Offset it a bit so we are less likely to hit an exact zero in log *)
    let epsilon = P.const 0.0000001 in
    let p1 = P.(p1 + epsilon) in
    let p2 = P.(p2 + epsilon) in
    let make_fun f = P.(func f [var 1; var 2]) in
    let f1 = define w p1 |> make_fun in
    let f2 = define w p2 |> make_fun in
    for n = 0 to num_steps do
      let alpha = float n /. float num_steps in
      make_frame w P.(scale f1 (1. -. alpha) + scale f2 alpha)
    done

  let emerge t l =
    let (p, ps) = t in
    let%bind { D. q; r = _ } = P.divide p l in
    let t' = (q, l :: ps) in
    return t'
end

let write ~dir ~config states =
  let%bind w = Writer.create ~dir ~config in
  let rec loop = function
    | s1 :: s2 :: states ->
      State.interpolate w s1 s2;
      loop (s2 :: states)
    | _ -> ()
  in
  loop states;
  return ()


