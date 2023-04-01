open Core
open Js_of_ocaml_lwt
open Std_internal
module PD = Probability_distribution
module V = Vector

module type Elt = sig
  module Id : Identifiable.S

  type t

  val id : t -> Id.t
  val offset : t -> t -> Vector.t
  val touch : t -> color:Color.t -> flash:bool -> unit
end

module Make (E : Elt) = struct
  module E = struct
    include E

    let compare_ids t1 t2 = Id.compare (id t1) (id t2)
    let distance t1 t2 = V.length (offset t1 t2)
  end

  module Id = Id (struct
    let name = "Rain"
  end)

  (*
  module Color = struct
    type t =
      { fst : Color.t
      ; snd : Color.t
    }

    let create (config : Config.Rain.Color.t) =
      match config.color with
      | This color -> { fst = color; snd = color }
      | Any ->
        let color =
          Color.random_interesting () |> Color.maximize
        in
        { fst = color; snd = color }
  end
*)

  type t =
    { id : Id.t
    ; config : Config.Rain.t
    ; color : Color.t
    ; elts : E.t list
    ; centre : E.t
    ; wind : V.t
    ; next_strand : E.t PD.t
    ; mutable last_drop : E.t
    ; mutable num_drops : int
    ; mutable centre_drops : int
    ; step : float
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
    List.filter_map elts ~f:(fun e ->
      PD.Elt.create_exn e ~weight:(weight e))
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

  let create_exn
    ~other_rains
    ~step
    ~elts
    ~(id : Id.t)
    ~(config : Config.Rain.t)
    =
    match elts with
    | [] | [ _ ] -> failwith "not enough rain elements"
    | elts ->
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
      { id
      ; config
      ; color
      ; elts
      ; next_strand
      ; last_drop = centre
      ; centre
      ; wind
      ; step
      ; centre_drops = 0
      ; num_drops = 0
      }
  ;;

  let next_strand_drop t =
    let open Float in
    let last_drop = t.last_drop in
    let raw_weight e =
      let v = E.offset e last_drop in
      let d = Float.max t.step V.(length (v - t.wind)) in
      (1. / d) **. t.config.wind_dropoff
    in
    let weight e = if phys_equal e last_drop then 0. else raw_weight e in
    let elts =
      List.filter_map t.elts ~f:(fun e ->
        let weight = weight e in
        PD.Elt.create_exn e ~weight)
    in
    let max_weight =
      List.map elts ~f:PD.Elt.weight
      |> List.max_elt ~compare:Float.compare
      |> Option.value_exn ~here:[%here]
    in
    if Float.(max_weight = raw_weight last_drop)
    then (* We hit the wall, start a new strand *) PD.draw t.next_strand
    else PD.draw (PD.create_exn elts)
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
    E.touch e ~color:t.color ~flash
  ;;

  let burst t ~drops_at_once =
    let open Lwt.Let_syntax in
    let rec loop () =
      let stop () = Lwt.return () in
      let%bind () =
        Lwt_js.sleep (Time.Span.to_sec t.config.drop_interval)
      in
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
end
