(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common
open Geometry

module Image : sig
  type t

  val load : string -> t Lwt.t
end

module Image_source : sig
  type t

  val image  : Image.t                 -> t
  val canvas : Html.canvasElement Js.t -> t
  val video  : Html.videoElement  Js.t -> t
end

module Ctx : sig
  type t

  val create : id:string -> t

  val canvas_actions : t -> Action.t Lwt_stream.t

  val clear : t -> unit

  val width : t -> float
  val height : t -> float

  val draw_circle : t -> Vector.t -> radius:float -> unit

  val draw_horizontal_line : t -> Vector.t -> unit
  val draw_vertical_line   : t -> Vector.t -> unit

  val fill_rect : t -> Vector.t -> width:float -> height:float -> unit
  val fill_all : t -> unit

  val draw_centered_rectangle : t -> width:float -> height:float -> unit

  val set_fill_color : t -> Color.t -> unit
  val set_stroke_color : t -> Color.t -> unit
  val set_line_width : t -> float -> unit
  val set_line_join : t -> [ `round | `bevel | `miter ]  -> unit
  val set_line_cap  : t -> [ `round | `butt  | `square ] -> unit

  val clip_rect : t -> Vector.t -> width:int -> height:int -> unit

  val set_font : t -> string -> unit
  val fill_text : t -> string -> float -> float -> unit
  val stroke_text : t -> string -> float -> float -> unit

  val begin_path : t -> unit
  val move_to : t -> Vector.t -> unit
  val line_to : t -> Vector.t -> unit
  val bezier_curve_to
    :  t
    -> control1:Vector.t
    -> control2:Vector.t
    -> Vector.t
    -> unit
  val stroke : t -> unit
  (* Preserve line width under transformations. *)
  val stroke_without_transform : t -> unit
  val clip : t -> unit

  val save : t -> unit
  val restore : t -> unit

  val set_transform : t -> Matrix.t -> unit
  val transform     : t -> Matrix.t -> unit

  val draw : t -> Image_source.t -> Vector.t -> unit
end

module Video : sig
  type t

  val create : id:string -> t

  (*
    - Doesn't work if you access the html over file://
    - In chrome requires that you access it over https:// or localhost.
      https://sites.google.com/a/chromium.org/dev/Home/chromium-security/deprecating-powerful-features-on-insecure-origins

    - To deactivate a camera:
      echo 1-7 > /sys/bus/usb/drivers/usb/unbind
   *)
  val read_camera : t -> unit

  (* CR-someday: maybe return the pixel array straight away? *)
  val get_frame : ?delay:bool -> t -> Ctx.t -> unit Lwt.t
end

val set_reload_on_resize : unit -> unit
