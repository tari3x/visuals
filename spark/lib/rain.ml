open Base
open Js_of_ocaml_lwt
open Std_internal
module PD = Probability_distribution
module V = Vector

module type Elt = sig
  module Id : Id

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

  type t =
    { id : Id.t
    ; config : Config.Rain.t
    ; color : Color.t
    ; elts : E.t list
    ; centre : E.t
    ; wind : V.t
    ; next_strand : E.t PD.t
    ; mutable last_drop : E.t
    ; mutable centre_drops : int
    ; min_distance : float
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
      else List.map cs ~f:(E.distance e) |> List.reduce_exn ~f:Float.( + )
    in
    List.map elts ~f:(fun e -> PD.Elt.create e ~weight:(weight e))
    |> PD.create_exn
    |> PD.draw
  ;;

  let random_drop_around_centre ~min_distance centre elts ~dropoff =
    let open Float in
    let other_elts =
      List.filter elts ~f:(fun e -> not (phys_equal e centre))
      |> List.map ~f:(fun e ->
             let distance = E.distance centre e in
             let weight = (min_distance / distance) **. dropoff in
             PD.Elt.create e ~weight)
    in
    PD.create_exn (PD.Elt.create centre ~weight:1. :: other_elts)
  ;;

  let create_exn
      ~other_rains
      ~min_distance
      ~elts
      ~(id : Id.t)
      ~(config : Config.Rain.t)
    =
    let color = Color.random_interesting () |> Color.maximize in
    let centre = choose_new_centre_exn ~elts ~other_rains in
    let wind = V.(scale (random_unit ()) ~by:min_distance) in
    let next_strand =
      random_drop_around_centre
        centre
        elts
        ~min_distance
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
    ; min_distance
    ; centre_drops = 0
    }
  ;;

  let next_strand_drop t =
    let open Float in
    let last_drop = t.last_drop in
    let weight e =
      let v = E.offset e last_drop in
      let d = Float.max 1. V.(length (v - t.wind)) in
      (1. / d) **. t.config.wind_dropoff
    in
    let elts =
      List.map t.elts ~f:(fun e ->
          let weight = weight e in
          PD.Elt.create e ~weight)
    in
    let max_weight =
      List.map elts ~f:PD.Elt.weight
      |> List.max_elt ~compare:Float.compare
      |> Option.value_exn
    in
    if Float.(max_weight = weight last_drop)
       (* We hit the wall, start a new strand *)
    then PD.draw t.next_strand
    else PD.draw (PD.create_exn elts)
  ;;

  let drop t ~flash =
    let e =
      let open Float in
      if Random.float 1. <= t.config.new_strand_probability
      then PD.draw t.next_strand
      else next_strand_drop t
    in
    t.last_drop <- e;
    if phys_equal e t.centre then t.centre_drops <- t.centre_drops + 1;
    E.touch e ~color:t.color ~flash
  ;;

  let burst t =
    let open Lwt.Let_syntax in
    let rec loop ~is_first =
      let open Float in
      let stop () = Lwt.return () in
      let%bind () =
        Lwt_js.sleep (Time.Span.to_sec t.config.drop_interval)
      in
      drop t ~flash:is_first;
      if Random.float 1. > t.config.keep_raining_probability
      then stop ()
      else loop ~is_first:false
    in
    loop ~is_first:true
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
