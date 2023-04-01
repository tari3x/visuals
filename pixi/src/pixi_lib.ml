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

module Renderer = struct
  include Renderer

  let resize t w h = t##resize w h
end

module Application = struct
  include Application

  let view t = t##.view
  let stage t = t##.stage
  let resize t window = t##resize window
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
  let draw_circle t ~x ~y ~radius = t##drawCircle ~x ~y ~radius

  let begin_fill (t : t) ~(color : Color.t) ?(alpha = 1.) () =
    t##beginFill ~color ~alpha
  ;;

  let end_fill t = t##endFill
  let move_to t x y = t##moveTo x y
  let line_to t x y = t##lineTo x y
  let close_path t = t##closePath

  let line_style
    (t : t)
    ?(width = 0.)
    ?(color = Color.white)
    ?(alpha = 1.)
    (* ?alignment
           ?native
      *)
      ()
    =
    (*
    let params =
    LineStyleParams.create ?width ?color ?alpha ?alignment ?native ()
    in
    t##lineStyle params
  *)
    t##lineStyle ~width ~color ~alpha
  ;;

  let set_matrix t m = t##setMatrix m
end
