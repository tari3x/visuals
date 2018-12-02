open Core
open Async

module Expr = struct
  type t = string [@@deriving sexp]

  let to_string t = t

  let to_gnuplot t =
    String.substr_replace_all t ~pattern:"^" ~with_:"**"

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

  let ( / ) t1 t2 =
    sprintf "(%s) / (%s)" t1 t2

  let ( + ) t1 t2 =
    sprintf "(%s) + (%s)" t1 t2

  let ( - ) t1 t2 =
    sprintf "(%s) - (%s)" t1 t2

  let scale t x =
    sprintf "%f * (%s)" x t

  let exp t power =
    sprintf "(%s)**%d" t power

  let product = function
    | [] -> const 1.
    | ts -> List.reduce_exn ts ~f:( * )

  let sum = function
    | [] -> const 0.
    | ts -> List.reduce_exn ts ~f:( + )

  let ev t ts =
    List.mapi ts ~f:(fun i t ->
      sprintf "%s = %s" (var Int.(i + 1)) t)
    |> String.concat ~sep:", "
    |> sprintf "ev(%s, %s)" t

  let expand t =
    sprintf "expand(%s)" t

  let eval_list expr =
    let tmp_in = Filename.temp_file "maxima" "" in
    let tmp_out = Filename.temp_file "maxima" "" in
    let cmds =
    (* "display2d:false" *)
    (* "ttyoff:true" *)
      [ (* sprintf "ratmx:false" *)
        sprintf "ratexpand:true"
      ; sprintf "stringout(\"%s\", %s);" tmp_out expr
      ]
      |> String.concat ~sep:"; "
    in
    let%bind () =
      Writer.with_file tmp_in ~f:(fun writer ->
        Writer.write_line writer cmds;
        return ())
    in
    let%bind output =
      Async_shell.run_full "bash"
        [ "-c"
        ; sprintf "maxima -b %s" tmp_in ]
    in
    let%bind result = Reader.file_contents tmp_out in
    let%bind () = Unix.unlink tmp_in in
    let%bind () = Unix.unlink tmp_out in
    if String.is_empty (String.strip result)
    then failwithf "maxima failed:\ninput:%s\noutput:%s\n" expr output ();
    let result = String.split_on_chars result ~on:['['; ']'; ','; ';'] in
    List.filter_map result ~f:(fun s ->
      match String.strip s with
      | "" -> None
      | s -> Some s)
    |> return

  let eval expr =
    match%bind eval_list expr with
    | [result] -> return result
    | _ -> failwith "unexpected number of maxima results"

  (* quotient and remainder *)
  module Division_result = struct
    type nonrec t = { q : t; r : t }
  end

  let divide t1 t2 =
    match%bind eval_list (sprintf "divide( %s, %s )" t1 t2) with
    | [ q; r ] -> return { Division_result. q; r }
    | _ -> assert false
end

module Matrix = struct
  type t = string [@@deriving sexp]

  let create rows =
    List.map rows ~f:(fun row ->
      String.concat row ~sep:", "
      |> sprintf "[%s]")
    |> String.concat ~sep:", "
    |> sprintf "matrix(%s)"

  let det t =
    (* sprintf "newdet(%s)" t *)
    sprintf "newdet(%s)" t

  let eval t =
    Expr.eval t
end
