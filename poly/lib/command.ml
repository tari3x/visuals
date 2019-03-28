open Core
open Async

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

let draw =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
     let dir = flag "-dir" (required Filename.arg_type) ~doc:"" in
     fun () ->
       Grid.animate ~dir
          (*
       Lagrange.eval_test ();
       Async.return ()
          *)
    ]

let window =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
     let () = return () in
     fun () ->
       let open Async in
       Window.run ();
       return ()
    ]

let command =
  Command.group
    ~summary:""
    [ "draw", draw
    ; "window", window
    ]