open Core
open Async
module P = Polynomial

let config =
  Config.create
    ~grid_size:(10, 10)
    ~cbrange:(-5., 5.)
    ~image_width:1024
    ~rendering_degree:3 (* 5 blows over SSBO limit  *)
    ~speed:10.
    ()
;;

let rec loop ctx =
  let%bind () = Clock.after (sec 0.001) in
  Draw.draw ctx P.(pow (x - const 5.) 2 + pow (y - const 5.) 3);
  match Draw.poll_event ctx with
  | Some Quit -> return ()
  | _ -> loop ctx
;;

let main () =
  let ctx = Draw.create config in
  loop ctx
;;

let command =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
      let () = return () in
      fun () -> main ()]
;;
