open Core
open Std_internal

module A = Animation
module P = Polynomial

let debug a = debug ~enabled:true a

let file_id = ref 0

let color =
  Colors.gnuplot_color

(* CR: zeroes still happen. *)
let make_frame ~dir ~config ~cache p =
  let open Float in
  let
      { Config.
        n_x = _
      ; n_y = _
      ; grid_size = (width, height)
      ; left_margin = _
      ; top_margin= _
      ; right_margin = _
      ; bottom_margin = _
      ; style = _
      ; cbrange = (min_value, max_value)
      ; show_dots = _
      ; degree = _
      } = config
  in
  incr file_id;
  let filename = dir ^/ sprintf "frame%06d.png" !file_id in
  let black = {Color.r = 0; g = 0; b = 0; } in
  let value_range = max_value - min_value in
  let image = Rgb24.make width height black in
  let values = P.eval_on_grid p ~cache in
  for i = 0 to Int.(width - 1) do
    for j = 0 to Int.(height - 1) do
      (* use mathematical orientation. *)
      let j = Int.(height - 1 - j) in
      let p_value = A2.get values i j in
      let sign = robust_sign p_value |> Sign.to_float in
      let color =
        if p_value = 0. then color 0.
        else begin
          let value = sign * log (abs p_value) in
          let value = max value min_value in
          let value = min value max_value in
          let w = (value - min_value) / value_range in
          color w
        end
      in
      Rgb24.set image i j color;
    done;
  done;
  Png.save filename [] (Images.Rgb24 image)

let write ~dir { A. config; states } =
  let cache = P.Mono_cache.create ~config ~degree:config.degree in
  debug !"Created cache at %{Time}" (Time.now ());
  List.iter states ~f:(fun { A.State. p; ps; defs = _; dots = _ } ->
    let p = P.product (p :: ps) in
    make_frame ~dir ~config ~cache p);
  Async.return ()
