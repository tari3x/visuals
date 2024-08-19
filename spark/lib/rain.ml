open Core
open Js_of_ocaml_lwt
open Std_internal
module PD = Probability_distribution
module V = Vector
module Shape = Shapes.Elt

module E = struct
  include Shape

  let offset t1 t2 =
    let open Vector in
    centre t1 - centre t2
  ;;

  let compare_ids t1 t2 = Id.compare (id t1) (id t2)
  let distance t1 t2 = V.length (offset t1 t2)
end

module Id = Id (struct
  let name = "Rain"
end)

type t =
  { id : Id.t
  ; config : Config.Rain.t
  ; color : Color.t
  ; shapes : Shapes.t
  ; centre : E.t
  ; wind : V.t
  ; next_strand : E.t PD.t
  ; next_drop : E.t PD.t option E.Id.Table.t
  ; mutable last_drop : E.t
  ; mutable num_drops : int
  ; mutable centre_drops : int
  ; touch : E.t -> color:Color.t -> flash:bool -> unit
  }
[@@deriving fields]

let choose_new_centre_exn ~elts ~other_rains =
  let cs =
    List.map other_rains ~f:centre
    |> List.dedup_and_sort ~compare:E.compare_ids
  in
  let is_degenerate =
    List.is_empty cs || List.length cs >= List.length elts
  in
  let weight e =
    if is_degenerate
    then 1.
    else if List.mem cs e ~equal:phys_equal
    then 0.
    else
      let open Float in
      let sum =
        List.map cs ~f:(E.distance e) |> List.reduce_exn ~f:Float.( + )
      in
      max sum 1.
  in
  List.filter_map elts ~f:(fun e -> PD.Elt.create_exn e ~weight:(weight e))
  |> PD.create_exn
  |> PD.draw
;;

let random_drop_around_centre ~step centre elts ~dropoff =
  let open Float in
  let other_elts =
    List.filter elts ~f:(fun e -> not (phys_equal e centre))
    |> List.filter_map ~f:(fun e ->
         let distance = max step (E.distance centre e) in
         let weight = (step / distance) **. dropoff in
         PD.Elt.create_exn e ~weight)
  in
  (PD.Elt.create_exn centre ~weight:1. |> Option.value_exn ~here:[%here])
  :: other_elts
  |> PD.create_exn
;;

let next_drop t =
  let open Float in
  let last_drop = t.last_drop in
  let step = Shapes.step t.shapes in
  Hashtbl.find_or_add t.next_drop (E.id last_drop) ~default:(fun () ->
    let elts =
      let centre = V.(E.centre last_drop + t.wind) in
      let rect =
        let diag = V.create_float (3. * step) (3. * step) in
        Rectangle.create_corners V.(centre - diag) V.(centre + diag)
      in
      Shapes.find_rect t.shapes rect
    in
    let raw_weight e =
      let v = E.offset e last_drop in
      let d = Float.max step V.(length (v - t.wind)) in
      if d > 3. * step then 0. else (1. / d) **. t.config.wind_dropoff
    in
    let weight e = if phys_equal e last_drop then 0. else raw_weight e in
    let elts =
      List.filter_map elts ~f:(fun e ->
        let weight = weight e in
        PD.Elt.create_exn e ~weight)
    in
    let max_weight =
      List.map elts ~f:PD.Elt.weight
      |> List.max_elt ~compare:Float.compare
      |> Option.value_exn ~here:[%here]
    in
    if Float.(max_weight = raw_weight last_drop)
    then (* We hit the wall, start a new strand *) None
    else Some (PD.create_exn elts))
;;

let create_exn
  ~(id : Id.t)
  ~(config : Config.Rain.t)
  ~other_rains
  ~shapes
  ~touch
  =
  let elts = Shapes.elts shapes |> Map.data in
  let step = Shapes.step shapes in
  let color =
    match config.color with
    | Choose colors -> List.random_element_exn colors
    | Any -> Color.random_interesting () |> Color.maximize
  in
  let centre = choose_new_centre_exn ~elts ~other_rains in
  let wind = V.(scale (random_unit ()) ~by:step) in
  let next_strand =
    random_drop_around_centre
      centre
      elts
      ~step
      ~dropoff:config.rain_dropoff
  in
  let next_drop = E.Id.Table.create () in
  { id
  ; config
  ; color
  ; shapes
  ; next_strand
  ; next_drop
  ; last_drop = centre
  ; centre
  ; wind
  ; centre_drops = 0
  ; num_drops = 0
  ; touch
  }
;;

let next_strand_drop t =
  match next_drop t with
  | None -> PD.draw t.next_strand
  | Some next_drop -> PD.draw next_drop
;;

let drop t ~flash =
  let e =
    let open Float in
    if Random.float 1. <= t.config.new_strand_probability
    then PD.draw t.next_strand
    else next_strand_drop t
  in
  let flash =
    let open Float in
    flash || Random.float 1. < t.config.flash_probability
  in
  t.last_drop <- e;
  t.num_drops <- t.num_drops + 1;
  if phys_equal e t.centre then t.centre_drops <- t.centre_drops + 1;
  t.touch e ~color:t.color ~flash
;;

let burst t ~drops_at_once =
  let open Lwt.Let_syntax in
  let rec loop () =
    let stop () = Lwt.return () in
    let%bind () = Lwt_js.sleep (Time.Span.to_sec t.config.drop_interval) in
    for _ = 1 to drops_at_once do
      let flash = t.config.flash_first && t.num_drops = 0 in
      drop t ~flash
    done;
    if Float.(Random.float 1. > t.config.keep_raining_probability)
    then stop ()
    else loop ()
  in
  loop ()
;;

(* This should be roughly enough to saturate the color *)
let saturation t =
  let open Float in
  of_int t.centre_drops * t.config.fade_to_base_interpolation_arg
;;

let sexp_of_t t =
  let saturation = saturation t in
  let color = t.color in
  [%message (saturation : float) (color : Color.t)]
;;
