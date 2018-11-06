open Core
open Async

type t = string

(* CR-someday: does gnuplot break if I use other variable names? *)
let var = function
  | 1 -> "x"
  | 2 -> "y"
  | 3 -> "z"
  | i -> sprintf "v%d" i

let func f args =
  sprintf "%s(%s)" f (String.concat args ~sep:", ")

let const = Float.to_string

let ( * ) t1 t2 =
  sprintf "(%s) * (%s)" t1 t2

let ( + ) t1 t2 =
  sprintf "(%s) + (%s)" t1 t2

let ( - ) t1 t2 =
  sprintf "(%s) - (%s)" t1 t2

let scale t x =
  sprintf "%f * (%s)" x t

let product = function
  | [] -> const 1.
  | ts -> List.reduce_exn ts ~f:( * )

(* quotient and remainder *)
module Division_result = struct
  type nonrec t = { q : t; r : t }
end

let _maxima_marker_regex =
  let open Re2 in
  Regex.create_exn "\\(%[io]\\d+\\)"

let eval_maxima expr =
  let tmp_in = Filename.temp_file "maxima" "" in
  let tmp_out = Filename.temp_file "maxima" "" in
  let cmds =
    (* "display2d:false" *)
    (* "ttyoff:true" *)
    [ sprintf "stringout(\"%s\", %s);" tmp_out expr
    ]
    |> String.concat ~sep:"; "
  in
  let%bind () =
    Writer.with_file tmp_in ~f:(fun writer ->
      Writer.write_line writer cmds;
      return ())
  in
  let%bind () =
    Async_shell.run "bash"
      [ "-c"
      ; sprintf "maxima -b %s" tmp_in ]
  in
  let%bind result = Reader.file_contents tmp_out in
  let%bind () = Unix.unlink tmp_in in
  let%bind () = Unix.unlink tmp_out in
  return result

let to_maxima t = t
let of_maxima t = t

let to_gnuplot t =
  String.substr_replace_all t ~pattern:"^" ~with_:"**"

let divide t1 t2 =
  let%bind output =
    eval_maxima
      (sprintf "divide( %s, %s )" (to_maxima t1) (to_maxima t2))
  in
  String.split_on_chars output ~on:['['; ']'; ','; ';']
  |> List.filter_map ~f:(fun s ->
    match String.strip s with
    | "" -> None
    | s -> Some s)
  |> (function
    | [ q; r ] ->
      let q = of_maxima q in
      let r = of_maxima r in
      { Division_result. q; r }
    | _ -> assert false)
  |> return

let zero_line_between_two_points (x1, y1) (x2, y2) =
  if Float.(x2 = x1)
  then var 1 - const x1
  else begin
    let slope = Float.((y2 - y1) / (x2 - x1)) in
    var 2 - const y1 - const slope * (var 1 - const x1)
  end
