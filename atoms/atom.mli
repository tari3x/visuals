(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

(* CR: change [Gui_full] to hide this definition. *)
type t =
| Rectangle
| Circle
| Horizontal_line
| Vertical_line
| Cross_line
| Zigzag
| Bezier
[@@deriving sexp]

val default : t

val render
  :  t Box.t
  -> Ctx.t
  -> time:float
  -> unit

val touched_by : t Box.t -> Vector.t -> bool

val scale_to_fit : t Box.t -> float -> t Box.t
