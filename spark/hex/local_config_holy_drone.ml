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
let blueish = Color.create ~r:140 ~g:140 ~b:255 ~a:1.
let almost_red = Color.create ~r:255 ~g:20 ~b:50 ~a:1.
let gray = Color.create ~r:130 ~g:130 ~b:130 ~a:1.
let () = ignore dark_orange
let () = ignore dark_blue
let () = ignore purple
let () = ignore blueish
let () = ignore almost_red
let () = ignore gray

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
(* CR avatar: upgrade jquery, they say some vulnerability *)
(* CR avatar: max size wire does crazy shit *)
(* CR avatar: window hangs after a little bit. Was it because of
   [Fade_to_base]? *)
(* CR avatar: - the middle hex is not evne in any way aligned with the shape
   hex  *)

let zoom = 1.
let new_strand_probability = 0.05
let rain = { Config.Rain.default with wind_dropoff = 1.5 }

let poly n size =
  let open Float in
  let open Geometry in
  let angle_step = 360. / Int.to_float n in
  let vertex i =
    let angle_deg = angle_step * float i in
    let angle_rad = pi / 180. * angle_deg in
    let d_x = size * cos angle_rad in
    let d_y = size * sin angle_rad in
    Vector.create_float d_x d_y
  in
  List.init n ~f:vertex |> Geometry.Shape.path
;;

let star_wire_shapes ~r1_mult =
  let open Float in
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
    [ hex (100. * mult)
    ; hex (100. * mult)
    ; hex (100. * mult)
    ; hex (100. * mult)
    ]
  in
  ignore condensed;
  let middle ~mult =
    [ square (70. * mult)
    ; hex (70. * mult)
    ; hex (70. * mult)
    ; hex (70. * mult)
    ; square (70. * mult)
    ]
  in
  let middle1 = middle ~mult:(3. * r1_mult) in
  let middle2 = middle ~mult:(3.8 * r1_mult) in
  let middle3 = middle ~mult:(6. * r1_mult) in
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
    ; twenty (100. * mult)
    ; twenty (110. * mult)
    ; twenty (120. * mult)
    ; twenty (130. * mult)
    ; twenty (140. * mult)
    ; twenty (150. * mult)
    ; twenty (160. * mult)
    ; twenty (170. * mult)
    ; twenty (180. * mult)
    ; twenty (190. * mult)
    ; twenty (200. * mult)
    ; twenty (200. * mult)
    ; twenty (210. * mult)
    ; twenty (220. * mult)
    ; twenty (230. * mult)
    ; twenty (240. * mult)
    ; twenty (250. * mult)
    ; twenty (260. * mult)
    ; twenty (270. * mult)
    ]
  in
  ignore circles;
  ignore middle;
  circles ~mult:1. @ circles ~mult:8.
;;

let star_bone_shapes ~r1_mult =
  let open Float in
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
    [ hex (100. * mult)
    ; hex (100. * mult)
    ; hex (100. * mult)
    ; hex (100. * mult)
    ]
  in
  ignore condensed;
  let middle ~mult =
    [ square (70. * mult)
    ; hex (70. * mult)
    ; hex (70. * mult)
    ; hex (70. * mult)
    ; square (70. * mult)
    ]
  in
  let middle1 = middle ~mult:(3. * r1_mult) in
  let middle2 = middle ~mult:(3.8 * r1_mult) in
  let middle3 = middle ~mult:(6. * r1_mult) in
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
    ; twenty (100. * mult)
    ; twenty (110. * mult)
    ; twenty (120. * mult)
    ; twenty (130. * mult)
    ; twenty (140. * mult)
      (*
    ; twenty (150. * mult)
    ; twenty (160. * mult)
    ; twenty (170. * mult)
    ; twenty (180. * mult)
    ; twenty (190. * mult)
    ; twenty (200. * mult)
    ; twenty (200. * mult)
    ; twenty (210. * mult)
    ; twenty (220. * mult)
    ; twenty (230. * mult)
    ; twenty (240. * mult)
    ; twenty (250. * mult)
    ; twenty (260. * mult)
    ; twenty (270. * mult)
        *)
    ]
  in
  ignore circles;
  ignore middle;
  middle ~mult:1.
  @ middle ~mult:3.
  @ middle ~mult:5.
  @ middle ~mult:7.
  @ middle ~mult:9.
  @ middle ~mult:11.
  @ middle ~mult:12.
;;

module Hex = struct
  type t =
    | Wire of Skin.t
    | Tile of Skin.t
    | Bone of Skin.t
    | Star of
        { skin : Skin.t
        ; shapes : Shape.t list
        }

  let map_skin t ~f =
    match t with
    | Wire skin -> Wire (f skin)
    | Tile skin -> Tile (f skin)
    | Bone skin -> Bone (f skin)
    | Star { skin; shapes } -> Star { skin = f skin; shapes }
  ;;

  let spark t ~r1_mult ~speed =
    let open Spark in
    match t with
    | Wire skin -> Hex_wire { skin; r1_mult }
    | Tile skin -> Hex_tile { skin; r1_mult }
    | Bone skin -> Hex_bone { skin; r1_mult }
    | Star { skin; shapes } ->
      Star { skin; shapes; speed; line_width = 4. }
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
    ; color = Choose [ almost_red; dark_blue ]
    }
  in
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 40.)
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
    ; color = Choose [ Color.white ]
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

let star_wire ~flash_mult ~intensity ~shapes =
  let open Float in
  let choose : Config.Rain.Color.t =
    (* Color.create ~r:255 ~g:50 ~b:80 ~a:1.; *)
    Choose [ Color.white ]
  in
  ignore choose;
  let rain =
    { rain with
      keep_raining_probability = 0.9 (* 0.6 *)
    ; rain_dropoff = 1.
    ; new_strand_probability
    ; flash_probability = 0.4
    ; color = Choose [ dark_orange; dark_orange; dark_blue ]
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
  Star { skin; shapes }
;;

let star_bone ~flash_mult ~intensity ~shapes =
  let open Float in
  let rain =
    { rain with
      new_strand_probability = 0.1
    ; keep_raining_probability = 0.9
    ; wind_dropoff = 2.
    ; rain_dropoff = 1.5
    ; flash_first = false
    ; color = Choose [ Color.white ]
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
  Star { skin; shapes }
;;

let off = Hex.map_skin ~f:(fun skin -> { skin with on_sound = None })

let config : C.t =
  let open Float in
  let _list_speed : Config.Spark.Speed.t = List [ 2.; 2.; 1. ] in
  let slow_speed : Config.Spark.Speed.t = Range { min = -2.; max = 2. } in
  let fast_speed : Config.Spark.Speed.t = Range { min = -5.; max = 5. } in
  let size1 = Hex.spark ~r1_mult:(zoom / 25.) ~speed:slow_speed in
  let size2 = Hex.spark ~r1_mult:(zoom / 15.) ~speed:slow_speed in
  let size3 = Hex.spark ~r1_mult:(zoom / 12.) ~speed:slow_speed in
  let size4 = Hex.spark ~r1_mult:(zoom / 9.) ~speed:slow_speed in
  (* CR avatar: remove the mult here, is not used. *)
  let star_size1 = Hex.spark ~r1_mult:(zoom * 1.) ~speed:slow_speed in
  let star_size2 = Hex.spark ~r1_mult:(zoom * 1.5) ~speed:fast_speed in
  ignore size2;
  ignore size3;
  ignore size4;
  ignore wire;
  ignore tile;
  ignore bone;
  let star =
    [ star_wire
        ~flash_mult:1.
        ~intensity:19.
        ~shapes:(star_wire_shapes ~r1_mult:(zoom * 1.))
      |> off
      |> star_size1
    ; star_bone
        ~flash_mult:0.1
        ~intensity:10
        ~shapes:(star_bone_shapes ~r1_mult:(zoom * 1.5))
      |> off
      |> star_size2
    ]
  in
  let size1 =
    [ wire ~flash_mult:0.15 ~intensity:15.
    ; tile ~flash_mult:0.4 ~intensity:1 |> off
    ; bone ~flash_mult:0.17 ~intensity:10 |> off
    ]
    |> List.map ~f:size1
  in
  let size2 =
    [ wire ~flash_mult:0.17 ~intensity:20.
    ; tile ~flash_mult:0.5 ~intensity:6 |> off
    ; bone ~flash_mult:0.4 ~intensity:350 |> off
    ]
    |> List.map ~f:size2
  in
  let size3 =
    [ wire ~flash_mult:0.35 ~intensity:3.
    ; tile ~flash_mult:0.05 ~intensity:15
    ; bone ~flash_mult:0.4 ~intensity:250 |> off
    ]
    |> List.map ~f:size3
  in
  let size4 =
    [ wire ~flash_mult:0.18 ~intensity:5.
    ; tile ~flash_mult:0.05 ~intensity:16
    ; bone ~flash_mult:0.8 ~intensity:20 |> off
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
