(*
   Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
   This file is distributed under a BSD license.
   See LICENSE file for copyright notice.
*)

open Std_internal
module C = Spark_lib.Config
open C

include struct
  include Color

  let dark_orange = Color.dark_orange
  let dark_blue = Color.create ~r:30 ~g:0 ~b:255 ~a:1.
  let dark_green = Color.create ~r:30 ~g:255 ~b:30 ~a:1.
  let purple = Color.create ~r:255 ~g:20 ~b:100 ~a:1.
  let blueish = Color.create ~r:140 ~g:140 ~b:255 ~a:1.
  let almost_red = Color.create ~r:255 ~g:20 ~b:50 ~a:1.
  let dark_red = Color.create ~r:200 ~g:20 ~b:20 ~a:1.
  let gray = Color.create ~r:130 ~g:130 ~b:130 ~a:1.
  let () = ignore dark_orange
  let () = ignore dark_blue
  let () = ignore purple
  let () = ignore blueish
  let () = ignore almost_red
  let () = ignore dark_red
  let () = ignore gray
  let () = ignore dark_green
end

(* CR-someday: the current config is wasteful when you just set things to zero.
   Problem is I can't remove tiles without restarting *)

(* CR-someday avatar: shape id collisions and global uniqueness *)
(* CR-someday avatar: you are setting canvas width in html? *)
(* CR-someday avatar: tile too agressive. Oh looks like Burst {drops_at_once} is just
   fucked, a single drop works fine. *)
(* CR-someday avatar: max bpm for tile *)
(* CR-someday avatar: the misalignment between the hexagons is what is ugly in bone. *)
(* CR-someday avatar: some bug in wire, too many colours, too many silent rains? *)
(* CR-someday avatar: play with perspective? *)
(* CR-someday avatar: must antialias when rotating *)
(* CR-someday avatar: allow shapes to be empty *)
(* CR-someday avatar: intersection with empty should not show! *)
(* CR-someday avatar: why does rotating hex not work? *)
(* CR-someday avatar: per-screen control *)
(* CR-someday avatar: line width doesn't update dynamically *)
(* CR-someday avatar: be able to duplicate the screen? *)
(* CR-someday avatar: goes white when reloading *)
(* CR-someday avatar: more efficiency, smaller hexes *)
(* CR-someday avatar: other star shapes *)
(* CR-someday avatar: make em stars fill the screen *)
(* CR-someday avatar: make hex fill the screen more *)
(* CR-someday avatar: reloading one window affects all, do I not have enough CPU? *)
(* CR-someday avatar: burst does seem better than drop for tile *)
(* CR-someday avatar: have some sort of health indicator *)
(* CR-someday avatar: upgrade jquery, they say some vulnerability *)
(* CR-someday avatar: max size wire does crazy shit *)
(* CR-someday avatar: window hangs after a little bit. Was it because of
   [Fade_to_base]? *)
(* CR-someday avatar: - the middle hex is not evne in any way aligned with the shape
   hex *)
(* CR-someday avatar: the small square pattern was literally not changing *)
(* CR-someday avatar: antialiasing *)
(* CR-someday avatar: make colours non-optonal *)
(* CR-someday avatar: things that are off never get deleted? *)
(* CR-someday avatar: the way I send colours to pixi is via string, really
   inefficient *)
(* CR-someday avatar: the screen should not be black for too long *)
(* CR-someday avatar: reloading causes many "ctl" updates in the log  *)
(* CR-someday avatar: the edge is only half wide. Use left margin? *)
(* CR-someday avatar: randomly flip between a finite set of speeds for all
   elements of a star, it creates a cool jumping around effect. *)
(* CR-someday avatar: play with alignments *)
(* CR avatar: star is fucked

   Remove [updateLocalTransform], is not used.

   https://github.com/pixijs/pixijs/issues/11444
*)
(* CR avatar: star bone needs fractional intensity *)
(* CR-someday avatar: fractional tile intensity *)
(* CR-someday avatar: still black lines.

   This is just hexes overlapping, but you could have a z-ordering where newer
   ones fully cover the old ones.
*)
(* CR avatar: it's too black for too long when music changes *)
(* CR avatar: some garbage collection is happening *)
(* CR avatar: individual hex control *)
(* CR avatar: permute maybe ex expensive? *)
(* CR avatar: it does freeze and jump occasionally *)
(* CR avatar: I'm no longer doing 60 frames? *)
(* CR avatar: big jump when reloading config *)
(* CR avatar: load_config seems to cause a page reload - sizes get
   reconfigured. *)
(* CR avatar: I need a better system for managing what is off. *)
(* CR avatar: there's a lot more frames per second when you're full width.  I
   guess it's just number of elements?
*)
(* CR-someday avatar: pixi js function allocates the most and shows up as
   useDeprecated.js,
   https://github.com/pixijs/pixijs/discussions/11746
*)
(* CR avatar:
   PixiJS Deprecation Warning: Application.view is deprecated, please use Application.canvas instead.Deprecated since v8.0.0
*)

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
  let middle ~mult = [ square (70. * mult); hex (70. * mult) ] in
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
  [ square 70. ]
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
  [ square 70. ]
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
    | None -> Choose [ Color.none ]
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
    ; segment_life_span = Time.Span.(of_sec 10.)
    ; flash_top = 1. * flash_mult
    ; flash_cutoff = 0.2 * flash_mult
    ; flash_duration = 0.4
    ; color_flow = Fade_to_none_smooth
    ; on_sound = Some (Wave { max_drops_per_second = intensity * 100. })
    ; max_sound_sources = 2
    }
  in
  Hex_wire skin
;;

let diamond_wire ?color ~flash_mult ~intensity () =
  let open Float in
  let color : Config.Rain.Color.t =
    match color with
    | Some color -> color
    | None -> Choose [ Color.none ]
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
    ; segment_life_span = Time.Span.(of_sec 10.)
    ; flash_top = 1. * flash_mult
    ; flash_cutoff = 0.35 * flash_mult
    ; flash_duration = 0.5
    ; color_flow = Fade_to_none_smooth
    ; on_sound = Some (Wave { max_drops_per_second = intensity * 100. })
    ; max_sound_sources = 2
    }
  in
  Diamond_wire skin
;;

let quad_wire ?color ~flash_mult ~intensity () =
  let open Float in
  let color : Config.Rain.Color.t =
    match color with
    | Some color -> color
    | None -> Choose [ Color.none ]
  in
  (* This one doesn't matter, right, cause it's rain? *)
  let rain =
    { rain with
      keep_raining_probability = 0.8 (* 0.6 *)
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
    ; on_sound = Some (Wave { max_drops_per_second = intensity * 100. })
    ; max_sound_sources = 2
    }
  in
  Quad_wire skin
;;

let hex_tile ?color ~flash_mult ~intensity () =
  let open Float in
  let color : Config.Rain.Color.t =
    match color with
    | Some color -> color
    | None -> Choose [ Color.none ]
  in
  let rain =
    { rain with
      new_strand_probability
    ; keep_raining_probability =
        0.9
        (* ; rain_dropoff = 1.
        *)
    ; wind_dropoff = 1.
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
    | None -> Choose [ Color.none ]
  in
  let rain =
    { rain with
      new_strand_probability
    ; keep_raining_probability = 0.9
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
    ; on_sound = Some (Beat (Burst { drops_at_once = intensity }))
    }
  in
  Diamond_tile skin
;;

let quad_tile ~color ~flash_mult ~intensity () =
  let open Float in
  let rain =
    { rain with
      new_strand_probability
    ; keep_raining_probability = 0.9
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
    ; on_sound =
        Some (Beat (Burst { drops_at_once = Float.to_int intensity }))
    }
  in
  Quad_tile skin
;;

let hex_bone ~flash_mult ~intensity ~color () =
  let open Float in
  let rain =
    { rain with
      new_strand_probability = 0.1
    ; keep_raining_probability = 0.9
    ; wind_dropoff = 1.
    ; rain_dropoff = 2.
    ; flash_probability = 1.
    ; flash_first = false
    ; color
    }
  in
  (* CR-someday: Fade_to_none seems more efficient than Fade_to_none_smooth? *)
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 3.)
    ; flash_top = 1. * flash_mult
    ; flash_cutoff = 0.4 * flash_mult
    ; flash_duration = 0.5
    ; color_flow = Fade_to_none
    ; max_sound_sources = 2
    ; on_sound = Some (Beat (Burst { drops_at_once = intensity }))
    }
  in
  Hex_bone skin
;;

let diamond_bone ~color ~flash_mult ~intensity () =
  let open Float in
  let rain =
    { rain with
      new_strand_probability = 0.1
    ; keep_raining_probability = 0.9
    ; wind_dropoff = 1.7
    ; rain_dropoff = 2.
    ; flash_probability = 1.
    ; flash_first = false
    ; color
    }
  in
  (* CR-someday: Fade_to_none seems more efficient than Fade_to_none_smooth? *)
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 3.)
    ; flash_top = 1. * flash_mult
    ; flash_cutoff = 0.4 * flash_mult
    ; flash_duration = 0.5
    ; color_flow = Fade_to_none
    ; max_sound_sources = 2
    ; on_sound = Some (Beat (Burst { drops_at_once = intensity }))
    }
  in
  Diamond_bone skin
;;

let quad_bone ~color ~flash_mult ~intensity () =
  let open Float in
  let rain =
    { rain with
      new_strand_probability = 0.1
    ; keep_raining_probability = 0.9
    ; wind_dropoff = 1.
    ; rain_dropoff = 2.
    ; flash_probability = 1.
    ; flash_first = false
    ; color
    }
  in
  (* CR-someday: Fade_to_none seems more efficient than Fade_to_none_smooth? *)
  let skin =
    { Config.Skin.default with
      rain
    ; segment_life_span = Time.Span.(of_sec 3.)
    ; flash_top = 1. * flash_mult
    ; flash_cutoff = 0.4 * flash_mult
    ; flash_duration = 0.5
    ; color_flow = Fade_to_none
    ; max_sound_sources = 2
    ; on_sound = Some (Beat (Burst { drops_at_once = intensity }))
    }
  in
  Quad_bone skin
;;

let star_wire ~flash_mult ~intensity ~shapes ~color =
  let open Float in
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
    ; flash_cutoff = 0.15 * flash_mult
    ; flash_duration = 0.5
    ; color_flow = Fade_to_none
    ; max_sound_sources = 2
    ; on_sound = Some (Beat (Drop intensity))
    }
  in
  Star { skin; shapes }
;;

let off =
  Layer.map_skin ~f:(fun skin ->
    { skin with on_sound = None; max_sound_sources = 0 })
;;

let config : C.t =
  let open Float in
  let _list_speed : Config.Spark.Speed.t = List [ 2.; 2.; 1. ] in
  let slow_speed : Config.Spark.Speed.t = Range { min = -5.; max = 5. } in
  let fast_speed : Config.Spark.Speed.t = Range { min = -5.; max = 6. } in
  let size1 = Layer.spark ~r1_mult:(zoom / 18.) ~speed:slow_speed in
  let size2 = Layer.spark ~r1_mult:(zoom / 12.) ~speed:slow_speed in
  let size3 = Layer.spark ~r1_mult:(zoom / 9.) ~speed:slow_speed in
  let size4 = Layer.spark ~r1_mult:(zoom / 6.) ~speed:slow_speed in
  let size5 = Layer.spark ~r1_mult:(zoom / 3.) ~speed:slow_speed in
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
      |> star_size1
    ; star_bone
        ~flash_mult:0.35
        ~intensity:1
        ~color:(Choose [ purple; dark_blue; dark_blue ])
        ~shapes:(star_bone_shapes ~r1_mult:(zoom * 1.5))
      |> off
      |> star_size2
    ]
  in
  let size1 =
    [ hex_wire
        ~flash_mult:1.
        ~intensity:10.
        ~color:(Choose [ dark_blue; purple ])
        ()
      |> off
    ; diamond_wire
        ~flash_mult:0.22
        ~intensity:1.3
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; quad_wire
        ~flash_mult:0.5
        ~intensity:1.5
        ~color:(Choose [ purple; dark_blue ])
        ()
      |> off
    ; quad_tile
        ~flash_mult:0.17
        ~intensity:25.
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; hex_tile ~flash_mult:0.18 ~intensity:60 ~color:(Choose [ purple ]) ()
      |> off
    ; diamond_tile
        ~flash_mult:0.15
        ~intensity:10
        ~color:(Choose [ purple ])
        ()
      |> off
    ; diamond_bone
        ~flash_mult:0.15
        ~intensity:100
        ~color:(Choose [ purple ])
        ()
      |> off
    ; hex_bone ~flash_mult:0.1 ~intensity:100 ~color:(Choose [ purple ]) ()
      |> off
    ; quad_bone ~flash_mult:0.7 ~intensity:50 ~color:(Choose [ purple ]) ()
      |> off
    ]
    |> List.map ~f:size1
  in
  let size2 =
    [ hex_wire
        ~flash_mult:1.
        ~intensity:10.
        ~color:(Choose [ dark_orange ])
        ()
      |> off
    ; diamond_wire
        ~flash_mult:0.22
        ~intensity:1.3
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; quad_wire
        ~flash_mult:0.5
        ~intensity:0.5
        ~color:(Choose [ purple; dark_blue ])
        ()
      |> off
    ; quad_tile
        ~flash_mult:0.17
        ~intensity:25.
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; hex_tile ~flash_mult:0.33 ~intensity:80 ~color:Any () |> off
    ; diamond_tile
        ~flash_mult:0.15
        ~intensity:10
        ~color:(Choose [ purple ])
        ()
      |> off
    ; diamond_bone
        ~flash_mult:0.3
        ~intensity:200
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; hex_bone ~flash_mult:0.7 ~intensity:50 ~color:(Choose [ purple ]) ()
      |> off
    ; quad_bone ~flash_mult:0.7 ~intensity:50 ~color:(Choose [ purple ]) ()
      |> off
    ]
    |> List.map ~f:size2
  in
  let size3 =
    [ hex_wire
        ~flash_mult:0.7
        ~intensity:16.
        ~color:(Choose [ dark_orange ])
        ()
    ; diamond_wire
        ~flash_mult:0.22
        ~intensity:10.
        ~color:(Choose [ dark_blue ])
        ()
    ; quad_wire
        ~flash_mult:0.8
        ~intensity:1.5
        ~color:(Choose [ purple; dark_blue ])
        ()
      |> off
    ; quad_tile
        ~flash_mult:0.2
        ~intensity:25.
        ~color:(Choose [ dark_blue; purple ])
        ()
      |> off
    ; hex_tile ~flash_mult:0.3 ~intensity:50 ~color:(Choose [ blueish ]) ()
      |> off
    ; diamond_tile
        ~flash_mult:0.15
        ~intensity:10
        ~color:(Choose [ purple ])
        ()
      |> off
    ; diamond_bone
        ~flash_mult:0.15
        ~intensity:100
        ~color:(Choose [ dark_blue; purple ])
        ()
      |> off
    ; hex_bone ~flash_mult:0.12 ~intensity:50 ~color:(Choose [ purple ]) ()
      |> off
    ; quad_bone ~flash_mult:0.7 ~intensity:50 ~color:(Choose [ purple ]) ()
      |> off
    ]
    |> List.map ~f:size3
  in
  let size4 =
    [ hex_wire
        ~flash_mult:1.
        ~intensity:0.2
        ~color:(Choose [ dark_orange ])
        ()
      |> off
    ; diamond_wire
        ~flash_mult:0.22
        ~intensity:1.3
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; quad_wire
        ~flash_mult:0.5
        ~intensity:1.5
        ~color:(Choose [ purple; dark_blue ])
        ()
      |> off
    ; quad_tile
        ~flash_mult:0.15
        ~intensity:40.
        ~color:(Choose [ blueish ])
        ()
      |> off
    ; hex_tile ~flash_mult:0.6 ~intensity:10 ~color:Any () |> off
    ; diamond_tile
        ~flash_mult:0.15
        ~intensity:10
        ~color:(Choose [ purple ])
        ()
      |> off
    ; diamond_bone
        ~flash_mult:0.6
        ~intensity:200
        ~color:(Choose [ dark_blue; purple ])
        ()
      |> off
    ; hex_bone
        ~flash_mult:1.
        ~intensity:200
        ~color:(Choose [ dark_blue; purple ])
        ()
      |> off
    ; quad_bone ~flash_mult:1. ~intensity:50 ~color:(Choose [ purple ]) ()
      |> off
    ]
    |> List.map ~f:size4
  in
  let size5 =
    [ hex_wire
        ~flash_mult:1.
        ~intensity:2.
        ~color:(Choose [ dark_orange ])
        ()
      |> off
    ; diamond_wire
        ~flash_mult:0.22
        ~intensity:1.3
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; quad_wire
        ~flash_mult:0.5
        ~intensity:1.5
        ~color:(Choose [ purple; dark_blue; dark_orange ])
        ()
      |> off
    ; quad_tile
        ~flash_mult:0.17
        ~intensity:25.
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; hex_tile
        ~flash_mult:0.8
        ~intensity:40
        ~color:(Choose [ purple; dark_orange ])
        ()
      |> off
    ; diamond_tile
        ~flash_mult:0.4
        ~intensity:20
        ~color:(Choose [ purple ])
        ()
      |> off
    ; diamond_bone
        ~flash_mult:0.3
        ~intensity:200
        ~color:(Choose [ dark_blue ])
        ()
      |> off
    ; hex_bone ~flash_mult:0.15 ~intensity:10 ~color:(Choose [ purple ]) ()
      |> off
    ; quad_bone ~flash_mult:0.2 ~intensity:30 ~color:(Choose [ purple ]) ()
      |> off
    ]
    |> List.map ~f:size5
  in
  { drawing_mode = false
  ; debug_sound = false
  ; calibration = Skip
  ; sparks = star @ size1 @ size2 @ size3 @ size4 @ size5
  ; global_channel_name = "global-hex"
  ; crop_top = 0.
  ; crop_bottom = 0.
  }
;;
