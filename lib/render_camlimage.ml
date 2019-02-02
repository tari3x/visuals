open Core
open Std_internal

module A = Animation
module P = Polynomial

let debug a = debug ~enabled:true a

let file_id = ref 0

let color =
  Colors.gnuplot_color

let draw_dot ~config image v =
  let x, y = Config.domain_to_image config v in
  let image_x, image_y = Config.image_size config in
  for i = x to x + 10 do
    for j = y to y + 10 do
      if 0 <= i && i < image_x && 0 <= j && j < image_y
      then Rgb24.set image i j Colors.red
    done
  done

let value_color ~(config : Config.t) p_value =
  let open Float in
  let (min_value, max_value) = Config.cbrange config in
  let value_range = max_value - min_value in
  (* Offset it a bit so we are less likely to hit an exact zero in log *)
  let p_value = p_value + 1e-7 in
  let sgn = robust_sign p_value |> Sign.to_float in
  if Float.(sgn = 0.) then color 0.
  else begin
    let value = sgn * log (abs p_value) in
    let value = max value min_value in
    let value = min value max_value in
    let w = (value - min_value) / value_range in
    color w
  end

(* CR: zeroes still happen. *)
let write_state
    ~dir ~(config : Config.t)
    ~cache { A.State. p; ps; defs = _; dots } =
  incr file_id;
  let (width, height) = Config.image_size config in
  let p = P.product (p :: ps) in
  let filename = dir ^/ sprintf "frame%06d.png" !file_id in
  let image = Rgb24.make width height Colors.black in
  let values = P.eval_on_grid p ~cache in
  for i = 0 to Int.(width - 1) do
    for j = 0 to Int.(height - 1) do
      (* use mathematical orientation. *)
      let j = Int.(height - 1 - j) in
      let p_value = A2.get values i j in
      let color = value_color ~config p_value in
      Rgb24.set image i j color;
    done;
  done;
  List.iter dots ~f:(fun dot ->
    let i, j = Config.domain_to_image config dot in
    let p_value = A2.get values i j in
    let color = value_color ~config p_value in
    debug !"%{Sexp}" [%message (p_value : float) (color : Color.t)];
    draw_dot ~config image dot
  );
  Png.save filename [] (Images.Rgb24 image)

let write ~dir { A. config; states } =
  let cache = P.Mono_cache.create ~config in
  List.iter states ~f:(write_state ~dir ~config ~cache);
  Async.return ()
