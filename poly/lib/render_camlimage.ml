open Core
open Async
open Std_internal
module A = Animation
module P = Polynomial

let _debug a = debug_s ~enabled:true a
let file_id = ref 0
let dot_size = 3

(* CR: this needs to be flipped. *)
let draw_dot ~config image v =
  let x, y = Config.domain_to_image config v in
  let image_x, image_y = Config.image_size config in
  for i = x - (dot_size / 2) to x + (dot_size / 2) do
    for j = y - (dot_size / 2) to y + (dot_size / 2) do
      if 0 <= i && i < image_x && 0 <= j && j < image_y
      then Rgb24.set image i j Color.Camlimages.red
    done
  done
;;

module Ctx = struct
  type t = { eval : Eval.Ctx.t }

  let create ~config =
    let eval = Eval.Ctx.create ~config in
    { eval }
  ;;
end

let write_image ~dir ~config ~values ~palette ?(dots = []) () =
  incr file_id;
  let palette = Array.map palette ~f:Color.camlimages in
  let filename = dir ^/ sprintf "frame%08d.png" !file_id in
  let width, height = Config.image_size config in
  let image = Rgb24.make width height Color.Camlimages.black in
  for i = 0 to Int.(width - 1) do
    for j = 0 to Int.(height - 1) do
      A2.get_flipped values i j |> Array.get palette |> Rgb24.set image i j
    done
  done;
  List.iter dots ~f:(fun dot ->
      (* This seems to be prone to off-by-ones if you put a dot right on the
       border *)
      (*
       let i, j = Config.domain_to_image config dot in
       debug [%message (i: int) (j : int)];
       let p_value = A2.get_flipped values i j in
       let color = palette.(p_value) in
       debug [%message (p_value : int) (color : Color.t)];
    *)
      draw_dot ~config image dot);
  Png.save filename [] (Images.Rgb24 image)
;;

(* CR: zeroes still happen. *)
let write_state
    ~dir
    ~(config : Config.t)
    ~(ctx : Ctx.t)
    { A.State.p; ps; defs = _; dots; palette }
  =
  let p = P.product (p :: ps) in
  let values = Eval.values ctx.eval p in
  let palette = Palette.create palette in
  write_image ~dir ~config ~values ~dots ~palette ()
;;

let write ~dir { A.config; states } =
  let ctx = Ctx.create ~config in
  List.iter states ~f:(write_state ~dir ~config ~ctx);
  Eval.Ctx.release ctx.eval;
  Async.return ()
;;

let command =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
      let file = anon ("file" %: Filename.arg_type)
      and dir = flag "-dir" (required Filename.arg_type) ~doc:"" in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind animation =
          Reader.load_sexp_exn file Animation.t_of_sexp
        in
        write ~dir animation]
;;
