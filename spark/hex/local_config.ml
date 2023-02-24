(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal
module C = Spark_lib.Config
open C

(* CR-someday: the current config is wasteful when you just set things to zero.
Problem is I can't remove tiles without restarting *)

(* CR avatar: make it possible to switch on to off without reloading *)
(* CR avatar: learn to make it less manic *)

let wire_intensity = 1.
let tile_intensity = 20.
let zoom = 1.
let new_strand_probability = 0.05
let rain = { Config.Rain.default with wind_dropoff = 1.5 }

module Hex = struct
  type t =
    | Wire of Skin.t
    | Tile of Skin.t
    | Bone of Skin.t

  let map_skin t ~f =
    match t with
    | Wire skin -> Wire (f skin)
    | Tile skin -> Tile (f skin)
    | Bone skin -> Bone (f skin)
  ;;

  let spark t ~r1_mult =
    let open Sparks in
    match t with
    | Wire skin -> Hex_wire { skin; r1_mult }
    | Tile skin -> Hex_tile { skin; r1_mult }
    | Bone skin -> Hex_bone { skin; r1_mult }
  ;;
end

open Hex

let wire ~flash_mult =
  let open Float in
  (* This one doesn't matter, right, cause it's rain? *)
  let rain =
    { rain with
      keep_raining_probability = 0.9 (* 0.6 *)
    ; rain_dropoff = 1.
    ; new_strand_probability
    ; flash_probability = 0.4
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
    ; on_sound =
        Some (Wave { max_drops_per_second = wire_intensity * 100. })
    ; max_sound_sources = 2
    ; base_color = Color.none
    }
  in
  Wire skin
;;

let tile ~flash_mult =
  let open Float in
  let rain =
    { rain with
      (* CR-someday: what is intensity doing here? *)
      new_strand_probability = tile_intensity * new_strand_probability
    ; keep_raining_probability = 0.95
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
    ; wind_dropoff = 5.
    ; rain_dropoff = 1.5
    ; flash_first = false
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

let off = Hex.map_skin ~f:(fun skin -> { skin with on_sound = None })

let config : C.t =
  let open Float in
  let size1 = Hex.spark ~r1_mult:(zoom * 0.02) in
  let size2 = Hex.spark ~r1_mult:(zoom * 0.04) in
  let size3 = Hex.spark ~r1_mult:(zoom * 0.06) in
  let size4 = Hex.spark ~r1_mult:(zoom * 0.08) in
  let size1 =
    [ wire ~flash_mult:0.2 |> off
    ; tile ~flash_mult:1.
    ; bone ~flash_mult:1.0 ~intensity:5 |> off
    ]
    |> List.map ~f:size1
  in
  let size2 =
    [ wire ~flash_mult:1. |> off
    ; tile ~flash_mult:0.1 |> off
    ; bone ~flash_mult:1. ~intensity:5 |> off
    ]
    |> List.map ~f:size2
  in
  let size3 =
    [ wire ~flash_mult:0.1 |> off
    ; tile ~flash_mult:0.2 |> off
    ; bone ~flash_mult:1.0 ~intensity:6 |> off
    ]
    |> List.map ~f:size3
  in
  let size4 =
    [ wire ~flash_mult:0.4 |> off
    ; tile ~flash_mult:0.2 |> off
    ; bone ~flash_mult:0.4 ~intensity:5 |> off
    ]
    |> List.map ~f:size4
  in
  { drawing_mode = false
  ; debug_sound = false
  ; calibration = Skip
  ; sparks = size1 @ size2 @ size3 @ size4
  ; global_channel_name = "global-hex"
  }
;;
