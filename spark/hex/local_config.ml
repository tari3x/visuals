(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal
module C = Spark_lib.Config
open C

let dark_orange = Color.dark_orange
let dark_blue = Color.create ~r:30 ~g:0 ~b:255 ~a:1.
let purple = Color.create ~r:255 ~g:20 ~b:100 ~a:1.
let () = ignore dark_orange
let () = ignore dark_blue
let () = ignore purple

(* CR-someday: the current config is wasteful when you just set things to zero.
   Problem is I can't remove tiles without restarting *)

(* CR avatar: shape id collisions and global uniqueness *)
(* CR avatar: you are setting canvas width in html? *)
(* CR avatar: tile too agressive. Oh looks like Burst {drops_at_once} is just
   fucked, a single drop works fine. *)
(* CR avatar: max bpm for tile *)
(* CR avatar: the misalignment between the hexagons is what is ugly in bone. *)
(* CR avatar: some bug in wire, too many colours, too many silent rains? *)
(* CR avatar: play with perspective? *)
(* CR avatar: must antialias when rotating *)
(* CR avatar: allow shapes to be empty *)
(* CR avatar: intersection with empty should not show! *)
(* CR avatar: why does rotating hex not work? *)
(* CR avatar: per-screen control *)
(* CR avatar: line width doesn't update dynamically *)
(* CR avatar: be able to duplicate the screen? *)
(* CR avatar: goes white when reloading *)
(* CR avatar: more efficiency, smaller hexes *)
(* CR avatar: other star shapes *)
(* CR avatar: make em stars fill the screen *)
(* CR avatar: make hex fill the screen more *)
(* CR avatar: reloading one window affects all, do I not have enough CPU? *)
(* CR avatar: burst does seem better than drop for tile *)
(* CR avatar: have some sort of health indicator *)

let zoom = 1.
let new_strand_probability = 0.05
let rain = { Config.Rain.default with wind_dropoff = 1.5 }

let star ~r1_mult =
  let open Float in
  let open Geometry in
  let poly n size =
    let angle_step = 360. / Int.to_float n in
    let vertex i =
      let angle_deg = angle_step * float i in
      let angle_rad = pi / 180. * angle_deg in
      let d_x = size * cos angle_rad in
      let d_y = size * sin angle_rad in
      Vector.create_float d_x d_y
    in
    List.init n ~f:vertex |> Geometry.Shape.path
  in
  let hex = poly 6 in
  let square = poly 4 in
  let tri = poly 3 in
  let five = poly 5 in
  let twenty = poly 30 in
  ignore hex;
  ignore square;
  ignore tri;
  ignore five;
  ignore twenty;
  let condensed ~mult =
    [ hex (100. * mult); hex (100. * mult); hex (100. * mult) ]
  in
  ignore condensed;
  let middle ~mult =
    [ square (40. * mult)
    ; hex (50. * mult)
    ; hex (70. * mult)
    ; hex (90. * mult)
    ; square (110. * mult)
    ; square (120. * mult)
    ; square (130. * mult)
    ; hex (150. * mult)
    ; hex (160. * mult)
    ; hex (170. * mult)
    ]
  in
  let middle1 = middle ~mult:(1. * r1_mult) in
  let middle2 = middle ~mult:(1.3 * r1_mult) in
  let middle3 = middle ~mult:(2. * r1_mult) in
  ignore middle1;
  ignore middle2;
  ignore middle3;
  let circles ~mult =
    [ twenty (10. * mult)
    ; twenty (20. * mult)
    ; twenty (30. * mult)
    ; twenty (40. * mult)
    ; twenty (50. * mult)
    ; twenty (60. * mult)
    ; twenty (70. * mult)
    ; twenty (80. * mult)
    ; twenty (90. * mult)
    ]
  in
  ignore circles;
  condensed ~mult:(7. * r1_mult)
;;

module Hex = struct
  type t =
    | Wire of Skin.t
    | Tile of Skin.t
    | Bone of Skin.t
    | Star of Skin.t

  let map_skin t ~f =
    match t with
    | Wire skin -> Wire (f skin)
    | Tile skin -> Tile (f skin)
    | Bone skin -> Bone (f skin)
    | Star skin -> Star (f skin)
  ;;

  let spark t ~r1_mult ~speed =
    let open Spark in
    match t with
    | Wire skin -> Hex_wire { skin; r1_mult }
    | Tile skin -> Hex_tile { skin; r1_mult }
    | Bone skin -> Hex_bone { skin; r1_mult }
    | Star skin ->
      Star { skin; shapes = star ~r1_mult; speed; line_width = 7. }
  ;;
end

open Hex

let wire ~flash_mult ~intensity =
  let open Float in
  (* This one doesn't matter, right, cause it's rain? *)
  let rain =
    { rain with
      keep_raining_probability = 0.9 (* 0.6 *)
    ; rain_dropoff = 1.
    ; new_strand_probability
    ; flash_probability = 0.4
    ; color = Any
    }
  in
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 10.)
    ; flash_top = 1. * flash_mult
    ; flash_cutoff = 0.35 * flash_mult
    ; flash_duration = 0.5
    ; color_flow = Fade_to_none_smooth
    ; on_sound = Some (Wave { max_drops_per_second = intensity * 100. })
    ; max_sound_sources = 2
    ; base_color = Color.none
    ; num_silent_rains = 2
    }
  in
  Wire skin
;;

let tile ~flash_mult ~intensity =
  let open Float in
  let rain =
    { rain with
      new_strand_probability
    ; keep_raining_probability = 0.95
    ; color = Any
    }
  in
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 2.)
    ; flash_top = 0.5 * flash_mult
    ; flash_cutoff = 0.3 * flash_mult
    ; flash_duration = 0.5
    ; color_flow = Fade_to_none
    ; max_sound_sources = 2
    ; base_color = Color.none
    ; on_sound = Some (Beat (Burst { drops_at_once = intensity }))
    }
  in
  Tile skin
;;

let bone ~flash_mult ~intensity =
  let open Float in
  let rain =
    { rain with
      new_strand_probability = 0.1
    ; keep_raining_probability = 0.9
    ; wind_dropoff = 2.
    ; rain_dropoff = 1.5
    ; flash_first = false
    ; color = Any
    }
  in
  (* CR-someday: Fade_to_none seems more efficient than Fade_to_none_smooth?  *)
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 7.)
    ; flash_top = 1. * flash_mult
    ; flash_cutoff = 0.35 * flash_mult
    ; flash_duration = 0.5
    ; color_flow = Fade_to_none
    ; max_sound_sources = 2
    ; base_color = Color.none
    ; on_sound = Some (Beat (Burst { drops_at_once = intensity }))
    }
  in
  Bone skin
;;

let blueish = Color.create ~r:140 ~g:140 ~b:255 ~a:1.

let star_wire ~flash_mult ~intensity =
  let open Float in
  (* This one doesn't matter, right, cause it's rain? *)
  let choose : Config.Rain.Color.t =
    Choose [ Color.create ~r:255 ~g:50 ~b:80 ~a:1.; blueish ]
  in
  ignore choose;
  let rain =
    { rain with
      keep_raining_probability = 0.9 (* 0.6 *)
    ; rain_dropoff = 1.
    ; new_strand_probability
    ; flash_probability = 0.4
    ; color = Choose [ Color.dark_orange ]
    }
  in
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 10.)
    ; flash_top = 1. * flash_mult
    ; flash_cutoff = 0.35 * flash_mult
    ; flash_duration = 0.5
    ; color_flow = Fade_to_none_smooth
    ; on_sound = Some (Wave { max_drops_per_second = intensity })
    ; max_sound_sources = 2
    ; base_color = Color.none
    ; num_silent_rains = 2
    }
  in
  Star skin
;;

let star_bone ~flash_mult ~intensity =
  let open Float in
  let rain =
    { rain with
      new_strand_probability = 0.1
    ; keep_raining_probability = 0.9
    ; wind_dropoff = 2.
    ; rain_dropoff = 1.5
    ; flash_first = false
    ; color = Choose [ Color.dark_orange ]
    }
  in
  (* CR-someday: Fade_to_none seems more efficient than Fade_to_none_smooth?  *)
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 7.)
    ; flash_top = 1. * flash_mult
    ; flash_cutoff = 0.35 * flash_mult
    ; flash_duration = 0.5
    ; color_flow = Fade_to_none
    ; max_sound_sources = 2
    ; base_color = Color.none
    ; on_sound = Some (Beat (Drop intensity))
    }
  in
  Star skin
;;

let off = Hex.map_skin ~f:(fun skin -> { skin with on_sound = None })

let config : C.t =
  let open Float in
  let _list_speed : Config.Spark.Speed.t = List [ 2.; 2.; 1. ] in
  let slow_speed : Config.Spark.Speed.t =
    Range { min = -0.2; max = 0.5 }
  in
  let fast_speed : Config.Spark.Speed.t = Range { min = -3.; max = 3. } in
  let size1 = Hex.spark ~r1_mult:(zoom / 25.) ~speed:slow_speed in
  let size2 = Hex.spark ~r1_mult:(zoom / 15.) ~speed:slow_speed in
  let size3 = Hex.spark ~r1_mult:(zoom / 5.) ~speed:slow_speed in
  let size4 = Hex.spark ~r1_mult:(zoom / 3.) ~speed:slow_speed in
  let star_size1 = Hex.spark ~r1_mult:(zoom * 1.) ~speed:slow_speed in
  let star_size2 = Hex.spark ~r1_mult:(zoom * 0.5) ~speed:fast_speed in
  ignore size2;
  ignore size3;
  ignore size4;
  ignore wire;
  ignore tile;
  ignore bone;
  let star =
    [ star_wire ~flash_mult:0.3 ~intensity:4. |> off |> star_size1
    ; star_bone ~flash_mult:0.2 ~intensity:5 |> off |> star_size2
    ]
  in
  let size1 =
    [ wire ~flash_mult:0.3 ~intensity:7. |> off
    ; tile ~flash_mult:0.4 ~intensity:4 |> off
    ; bone ~flash_mult:0.2 ~intensity:20 |> off
    ]
    |> List.map ~f:size1
  in
  let size2 =
    [ wire ~flash_mult:0.1 ~intensity:10.
    ; tile ~flash_mult:0.2 ~intensity:3 |> off
    ; bone ~flash_mult:0.2 ~intensity:20 |> off
    ]
    |> List.map ~f:size2
  in
  let size3 =
    [ wire ~flash_mult:0.1 ~intensity:7. |> off
    ; tile ~flash_mult:0.35 ~intensity:6
    ; bone ~flash_mult:1. ~intensity:30
    ]
    |> List.map ~f:size3
  in
  let size4 =
    [ wire ~flash_mult:0.2 ~intensity:1. |> off
    ; tile ~flash_mult:0.35 ~intensity:15 |> off
    ; bone ~flash_mult:1. ~intensity:100 |> off
    ]
    |> List.map ~f:size4
  in
  { drawing_mode = false
  ; debug_sound = false
  ; calibration = Skip
  ; sparks = star @ size1 @ size2 @ size3 @ size4
  ; global_channel_name = "global-hex"
  }
;;
