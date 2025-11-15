(*
   Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
   This file is distributed under a BSD license.
   See LICENSE file for copyright notice.
*)

open Js_of_ocaml
open Raw

module DisplayObject = struct
  include DisplayObject
end

module Container = struct
  include Container

  let add_child t child = t##addChild child
end

module Application = struct
  include Application

  let init t = t##init
  let view t = t##.view
  let stage t = t##.stage
  let resize_to t window = t##.resizeTo := window
  let resize t = t##resize
  let renderer t = t##.renderer
end

module Color = struct
  include Color
end

module Matrix = struct
  include Matrix

  let from_array t array = t##fromArray (Js.array array)
end

module Graphics = struct
  include Graphics

  let clear (t : t) = t##clear
  let width t = t##.width
  let height t = t##.height

  let draw_circle (t : t) ~x ~y ~radius : unit =
    t##drawCircle ~x ~y ~radius
  ;;

  let draw_rect t ~x ~y ~width ~height = t##drawRect ~x ~y ~width ~height

  let fill (t : t) ~(color : Color.t) ?alpha () =
    t##fill (FillStyle.create ~color ?alpha ())
  ;;

  let stroke (t : t) ~(color : Color.t) ?alpha ?width () =
    t##stroke (StrokeStyle.create ~color ?alpha ?width ())
  ;;

  let move_to t x y = t##moveTo x y
  let line_to t x y = t##lineTo x y
  let close_path t = t##closePath
  let reset_transform t = t##resetTransform
  let transform t m = t##transform m
end
