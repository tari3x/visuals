open Core
open Async

module P = Polynomial
module D = P.Division_result
module V = Vector.Float

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
    ; show_dots : V.t list
    }
end

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
    fprintf w "set cbrange [%d:%d]\n" cb_x cb_y;
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

let define (w : Writer.t) p =
  incr fun_id;
  let f = sprintf "f%d" !fun_id in
  fprintf w.writer "%s(x, y) = %s\n" f (P.to_gnuplot p);
  f

let frame = ref 0

let make_frame { Writer. writer = w; config } f ~dots =
  incr frame;
  fprintf w "set output 'frame%06d.png'\n" !frame;
  let f = P.to_gnuplot f in
  Writer.add_dots w dots;
  match config.style with
  | `zeroes ->
    fprintf w "splot(%s)\n" f
  | `heat ->
    fprintf w "splot(sgn(%s) * log(abs(%s)))\n" f f

module State = struct
  (* animated and static parts *)
  type t =
    { p : P.t
    ; ps : P.t list
    ; dots : V.t list
    }

  let create ?(show_dots = []) p =
    { p; ps = []; dots = show_dots}

  let product t =
    P.product (t.p :: t.ps)

  let collapse t =
    { p = product t
    ; ps = []
    ; dots = t.dots
    }

  let interpolate w t1 t2 =
    let num_steps = 110 in
    let p1 = product t1 in
    let p2 = product t2 in
    (* Offset it a bit so we are less likely to hit an exact zero in log *)
    let epsilon = P.const 0.0000001 in
    let p1 = P.(p1 + epsilon) in
    let p2 = P.(p2 + epsilon) in
    let make_fun f = P.(func f [var 1; var 2]) in
    let f1 = define w p1 |> make_fun in
    let f2 = define w p2 |> make_fun in
    for n = 0 to num_steps - 1 do
      let alpha = float n /. float num_steps in
      make_frame w P.(scale f1 (1. -. alpha) + scale f2 alpha) ~dots:t1.dots
    done

  let emerge t l =
    let { p;  ps; _ } = t in
    let%bind { D. q; r = _ } = P.divide p l in
    { t with
      p = q
      ; ps = l :: ps
    }
    |> return
end

(* CR-someday: first interpolate, then write. *)
let write ~dir ~config ?(interpolate = true) states =
  let file_id = ref 0 in
  let%bind w = Writer.create ~dir ~config ~file_id:!file_id in
  (* CR-someday: it's weird that we don't use [define] in this code path. *)
  let write_all states =
    List.iter states ~f:(fun s ->
      make_frame w (State.product s) ~dots:s.dots);
    return ()
  in
  let rec loop w i = function
    | s1 :: s2 :: states ->
      State.interpolate w s1 s2;
      let states = s2 :: states in
      if i < 10
      then loop w (i + 1) states
      else begin
        incr file_id;
        let%bind w = Writer.create ~dir ~config ~file_id:!file_id in
        loop w 0 states
      end
    | _ -> return ()
  in
  if interpolate
  then loop w 0 states
  else write_all states
