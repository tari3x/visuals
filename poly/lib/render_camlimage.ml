open Core
open Async
open Std_internal

module A = Animation
module P = Polynomial

let debug a = debug_s ~enabled:true a

let file_id = ref 0

let draw_dot ~config image v =
  let x, y = Config.domain_to_image config v in
  let image_x, image_y = Config.image_size config in
  for i = x to x + 10 do
    for j = y to y + 10 do
      if 0 <= i && i < image_x && 0 <= j && j < image_y
      then Rgb24.set image i j Colors.red
    done
  done

module Ctx = struct
  type t =
    { eval : Eval.Ctx.t
    }

  let create ~config =
    let eval = Eval.Ctx.create ~config in
    { eval }
end

(* CR: zeroes still happen. *)
let write_state
    ~dir ~(config : Config.t)
    ~(ctx : Ctx.t)
    { A.State. p; ps; defs = _; dots } =
  incr file_id;
  let (width, height) = Config.image_size config in
  let p = P.product (p :: ps) in
  let filename = dir ^/ sprintf "frame%06d.png" !file_id in
  let image = Rgb24.make width height Colors.black in
  let values = Eval.values ctx.eval p in
  for i = 0 to Int.(width - 1) do
    for j = 0 to Int.(height - 1) do
      A2.get_flipped values i j
      |> Render.value_color
      |> Rgb24.set image i j;
    done;
  done;
  List.iter dots ~f:(fun dot ->
    let i, j = Config.domain_to_image config dot in
    let p_value = A2.get values i j in
    let color = Render.value_color p_value in
    debug [%message (p_value : int) (color : Color.t)];
    draw_dot ~config image dot
  );
  Png.save filename [] (Images.Rgb24 image)

let write ~dir { A. config; states } =
  let ctx = Ctx.create ~config in
  List.iter states ~f:(write_state ~dir ~config ~ctx);
  Eval.Ctx.release ctx.eval;
  Async.return ()

let command =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
     let file = anon ("file" %: Filename.arg_type)
     and dir = flag "-dir" (required Filename.arg_type) ~doc:""
     in
     fun () ->
       let open Deferred.Let_syntax in
       let%bind animation = Reader.load_sexp_exn file Animation.t_of_sexp in
       write ~dir animation
    ]
