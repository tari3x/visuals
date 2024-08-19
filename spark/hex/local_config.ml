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
let dark_green = Color.create ~r:30 ~g:255 ~b:30 ~a:1.
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
let () = ignore dark_green

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
   hex *)
(* CR avatar: fractional tile intensity *)
(* CR avatar: the small square pattern was literally not changing *)
(* CR avatar: antialiasing *)
(* CR avatar: make colours non-optonal *)
(* CR avatar: diamond bone is borken *)

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
  circles ~mult:1. @ circles ~mult:3. @ circles ~mult:7.
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
  middle ~mult:(r1_mult * 1.)
  @ middle ~mult:(r1_mult * 2.)
  @ middle ~mult:(r1_mult * 3.)
  @ middle ~mult:(r1_mult * 4.)
  @ middle ~mult:(r1_mult * 6.)
  @ middle ~mult:(r1_mult * 8.)
  @ middle ~mult:(r1_mult * 10.)
  @ middle ~mult:(r1_mult * 2.)
  @ middle ~mult:(r1_mult * 3.)
  @ middle ~mult:(r1_mult * 4.)
  @ middle ~mult:(r1_mult * 6.)
  @ middle ~mult:(r1_mult * 8.)
  @ middle ~mult:(r1_mult * 10.)
;;

module Layer = struct
  type t =
    | Hex_wire of Skin.t
    | Hex_tile of Skin.t
    | Hex_bone of Skin.t
    | Quad_wire of Skin.t
    | Quad_tile of Skin.t
    | Quad_bone of Skin.t
    | Diamond_wire of Skin.t
    | Diamond_tile of Skin.t
    | Diamond_bone of Skin.t
    | Star of
        { skin : Skin.t
        ; shapes : Shape.t list
        }

  let map_skin t ~f =
    match t with
    | Hex_wire skin -> Hex_wire (f skin)
    | Hex_tile skin -> Hex_tile (f skin)
    | Hex_bone skin -> Hex_bone (f skin)
    | Quad_wire skin -> Quad_wire (f skin)
    | Quad_tile skin -> Quad_tile (f skin)
    | Quad_bone skin -> Quad_bone (f skin)
    | Diamond_wire skin -> Diamond_wire (f skin)
    | Diamond_tile skin -> Diamond_tile (f skin)
    | Diamond_bone skin -> Diamond_bone (f skin)
    | Star { skin; shapes } -> Star { skin = f skin; shapes }
  ;;

  let spark t ~r1_mult ~speed : Spark.t =
    match t with
    | Hex_wire skin -> Hex_wire { skin; r1_mult }
    | Hex_tile skin -> Hex_tile { skin; r1_mult }
    | Hex_bone skin -> Hex_bone { skin; r1_mult }
    | Quad_wire skin -> Quad_wire { skin; r1_mult }
    | Quad_tile skin -> Quad_tile { skin; r1_mult }
    | Quad_bone skin -> Quad_bone { skin; r1_mult }
    | Diamond_wire skin -> Diamond_wire { skin; r1_mult }
    | Diamond_tile skin -> Diamond_tile { skin; r1_mult }
    | Diamond_bone skin -> Diamond_bone { skin; r1_mult }
    | Star { skin; shapes } ->
      Star { skin; shapes; speed; line_width = 4. }
  ;;
end

open Layer

let hex_wire ?color ~flash_mult ~intensity () =
  let open Float in
  let color : Config.Rain.Color.t =
    match color with
    | Some color -> color
    | None -> Choose [ Color.white ]
  in
  (* This one doesn't matter, right, cause it's rain? *)
  let rain =
    { rain with
      keep_raining_probability = 0.9 (* 0.6 *)
    ; rain_dropoff = 1.
    ; new_strand_probability
    ; flash_probability = 0.4
    ; color
    }
  in
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 7.)
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
  Hex_wire skin
;;

let diamond_wire ?color ~flash_mult ~intensity () =
  let open Float in
  let color : Config.Rain.Color.t =
    match color with
    | Some color -> color
    | None -> Choose [ Color.white ]
  in
  (* This one doesn't matter, right, cause it's rain? *)
  let rain =
    { rain with
      keep_raining_probability = 0.9 (* 0.6 *)
    ; rain_dropoff = 1.
    ; new_strand_probability
    ; flash_probability = 0.4
    ; color
    }
  in
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 5.)
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
  Diamond_wire skin
;;

let quad_wire ?color ~flash_mult ~intensity () =
  let open Float in
  let color : Config.Rain.Color.t =
    match color with
    | Some color -> color
    | None -> Choose [ Color.white ]
  in
  (* This one doesn't matter, right, cause it's rain? *)
  let rain =
    { rain with
      keep_raining_probability = 0.9 (* 0.6 *)
    ; rain_dropoff = 1.
    ; new_strand_probability
    ; flash_probability = 0.4
    ; color
    }
  in
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 20.)
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
  Quad_wire skin
;;

let hex_tile ?color ~flash_mult ~intensity () =
  let open Float in
  let color : Config.Rain.Color.t =
    match color with
    | Some color -> color
    | None -> Choose [ Color.white ]
  in
  let rain =
    { rain with
      new_strand_probability
    ; keep_raining_probability = 0.3
    ; color
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
  Hex_tile skin
;;

let diamond_tile ?color ~flash_mult ~intensity () =
  let open Float in
  let color : Config.Rain.Color.t =
    match color with
    | Some color -> color
    | None -> Choose [ Color.white ]
  in
  let rain =
    { rain with
      new_strand_probability
    ; keep_raining_probability = 0.5
    ; color
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
  Diamond_tile skin
;;

let quad_tile ~flash_mult ~intensity =
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
  Quad_tile skin
;;

let hex_bone
  ?(color = Config.Rain.Color.Choose [ Color.white ])
  ~flash_mult
  ~intensity
  ()
  =
  let open Float in
  let rain =
    { rain with
      new_strand_probability = 0.1
    ; keep_raining_probability = 0.9
    ; wind_dropoff = 2.
    ; rain_dropoff = 1.5
    ; flash_first = false
    ; color
    }
  in
  (* CR-someday: Fade_to_none seems more efficient than Fade_to_none_smooth? *)
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 7.)
    ; flash_top = 1. * flash_mult
    ; flash_cutoff = 0.2 * flash_mult
    ; flash_duration = 0.3
    ; color_flow = Fade_to_none
    ; max_sound_sources = 2
    ; base_color = Color.none
    ; on_sound = Some (Beat (Burst { drops_at_once = intensity }))
    }
  in
  Hex_bone skin
;;

let diamond_bone ~flash_mult ~intensity =
  let open Float in
  let rain =
    { rain with
      new_strand_probability = 0.1
    ; keep_raining_probability = 0.9
    ; wind_dropoff = 2.
    ; rain_dropoff = 1.5
    ; flash_first = false
    ; color = Choose [ dark_blue; purple; dark_orange ]
    }
  in
  (* CR-someday: Fade_to_none seems more efficient than Fade_to_none_smooth? *)
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
  Diamond_bone skin
;;

let quad_bone ~flash_mult ~intensity =
  let open Float in
  let rain =
    { rain with
      new_strand_probability = 0.1
    ; keep_raining_probability = 0.9
    ; wind_dropoff = 2.
    ; rain_dropoff = 1.5
    ; flash_first = false
    ; color = Choose [ dark_blue ]
    }
  in
  (* CR-someday: Fade_to_none seems more efficient than Fade_to_none_smooth? *)
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
  Quad_bone skin
;;

let star_wire ~flash_mult ~intensity ~shapes ~color =
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
    ; color
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

let star_bone ~flash_mult ~intensity ~shapes ~color =
  let open Float in
  let rain =
    { rain with
      new_strand_probability = 0.1
    ; keep_raining_probability = 0.9
    ; wind_dropoff = 2.
    ; rain_dropoff = 1.5
    ; flash_first = false
    ; color
    }
  in
  (* CR-someday: Fade_to_none seems more efficient than Fade_to_none_smooth? *)
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

let off = Layer.map_skin ~f:(fun skin -> { skin with on_sound = None })

let config : C.t =
  let open Float in
  let _list_speed : Config.Spark.Speed.t = List [ 2.; 2.; 1. ] in
  let slow_speed : Config.Spark.Speed.t = Range { min = -5.; max = 5. } in
  let fast_speed : Config.Spark.Speed.t = Range { min = -5.; max = 6. } in
  let size1 = Layer.spark ~r1_mult:(zoom / 15.) ~speed:slow_speed in
  let size2 = Layer.spark ~r1_mult:(zoom / 12.) ~speed:slow_speed in
  let size3 = Layer.spark ~r1_mult:(zoom / 9.) ~speed:slow_speed in
  let size4 = Layer.spark ~r1_mult:(zoom / 5.) ~speed:slow_speed in
  let size5 = Layer.spark ~r1_mult:(zoom / 4.) ~speed:slow_speed in
  (* CR avatar: remove the mult here, is not used. *)
  let star_size1 = Layer.spark ~r1_mult:(zoom * 1.) ~speed:slow_speed in
  let star_size2 = Layer.spark ~r1_mult:(zoom * 0.002) ~speed:fast_speed in
  ignore size2;
  ignore size3;
  ignore size4;
  ignore hex_wire;
  ignore hex_tile;
  ignore hex_bone;
  ignore quad_wire;
  ignore quad_tile;
  ignore quad_bone;
  ignore diamond_wire;
  ignore diamond_tile;
  ignore diamond_bone;
  let star =
    [ star_wire
        ~flash_mult:1.
        ~intensity:40.
        ~shapes:(star_wire_shapes ~r1_mult:(zoom * 2.1))
        ~color:(Choose [ dark_blue ])
      |> off
      |> star_size1
    ; star_bone
        ~flash_mult:0.3
        ~intensity:1
        ~color:(Choose [ purple ])
        ~shapes:(star_bone_shapes ~r1_mult:(zoom * 1.5))
      |> off
      |> star_size2
    ]
  in
  let size1 =
    [ hex_wire
        ~flash_mult:0.1
        ~intensity:6.
        ~color:(Choose [ dark_green ])
        ()
      |> off
    ; quad_wire ~flash_mult:0.1 ~intensity:0.1 () |> off
    ; hex_tile
        ~flash_mult:0.35
        ~intensity:10
        ~color:(Choose [ Color.white ])
        ()
      |> off
    ; hex_bone ~flash_mult:0.6 ~intensity:10 () |> off
    ]
    |> List.map ~f:size1
  in
  let size2 =
    [ hex_wire
        ~flash_mult:0.07
        ~intensity:3.
        ~color:(Choose [ dark_green ])
        ()
      |> off
    ; quad_wire ~flash_mult:0.07 ~intensity:0.3 () |> off
    ; hex_tile ~flash_mult:0.15 ~intensity:10 () |> off
    ; hex_bone ~flash_mult:0.2 ~intensity:250 () |> off
    ]
    |> List.map ~f:size2
  in
  let size3 =
    [ hex_wire
        ~flash_mult:0.05
        ~intensity:2.
        ~color:(Choose [ Color.white ])
        ()
      |> off
    ; quad_wire ~flash_mult:0.1 ~intensity:0.1 () |> off
    ; hex_tile ~flash_mult:0.1 ~intensity:1 () |> off
    ; hex_bone ~flash_mult:0.3 ~intensity:100 () |> off
    ]
    |> List.map ~f:size3
  in
  let size4 =
    [ hex_wire
        ~flash_mult:1.
        ~intensity:0.3
        ~color:(Choose [ dark_blue; purple; dark_orange ])
        ()
      |> off
    ; diamond_wire
        ~flash_mult:0.17
        ~intensity:5.
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; quad_wire ~flash_mult:0.1 ~intensity:0.05 () |> off
    ; hex_tile
        ~flash_mult:0.09
        ~intensity:40
        ~color:(Choose [ Color.white ])
        ()
      |> off
    ; diamond_tile
        ~flash_mult:0.45
        ~intensity:10
        ~color:(Choose [ purple; dark_blue ])
        ()
    ; hex_bone
        ~flash_mult:1.
        ~intensity:20
        ~color:(Choose [ dark_blue; purple; dark_orange ])
        ()
    ]
    |> List.map ~f:size4
  in
  let size5 =
    [ hex_wire
        ~flash_mult:0.45
        ~intensity:0.1
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; quad_wire ~flash_mult:0.03 ~intensity:0.04 () |> off
    ; hex_tile
        ~flash_mult:0.25
        ~intensity:3
        ~color:(Choose [ Color.white ])
        ()
      |> off
    ; hex_bone ~flash_mult:0.3 ~intensity:20 () |> off
    ]
    |> List.map ~f:size5
  in
  { drawing_mode = false
  ; debug_sound = false
  ; calibration = Skip
  ; sparks = star @ size1 @ size2 @ size3 @ size4 @ size5
  ; global_channel_name = "global-hex"
  }
;;
