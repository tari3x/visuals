open Js_of_ocaml
open Common
open Geometry
open Dom_wrappers

open (
  Pixi_lib : module type of Pixi_lib with module Color := Pixi_lib.Color)

module A = Application
module G = Graphics
module C = Container
open G

let color_to_pixi color =
  let r, g, b, alpha = Color.components color in
  Pixi_lib.Color.create r g b, alpha
;;

type t =
  { a : A.t
  ; g : G.t
  }

let init_exn () =
  let a = A.create () in
  let width = Window.(inner_width current) |> Option.value_exn in
  let height = Window.(inner_height current) |> Option.value_exn in
  Renderer.resize (A.renderer a) width height;
  let (_ : Dom.node Js.t) =
    Html.document##.body##appendChild (A.view a :> Dom.node Js.t)
  in
  let g = G.create () in
  C.add_child (A.stage a) (g :> DisplayObject.t);
  { a; g }
;;

let clear t = clear t.g
let width t = Canvas.width (A.view t.a)
let height t = Canvas.height (A.view t.a)
let draw_circle t = draw_circle t.g
let end_fill t = end_fill t.g
let close_path t = close_path t.g

let begin_fill (t : t) color =
  let color, alpha = color_to_pixi color in
  begin_fill t.g ~color ~alpha ()
;;

let move_to t v =
  let x, y = Vector.coords v in
  move_to t.g x y
;;

let line_to t v =
  let x, y = Vector.coords v in
  line_to t.g x y
;;

let path (t : t) ~closed vs =
  match vs with
  | [] -> ()
  | v :: vs ->
    move_to t v;
    List.iter vs ~f:(line_to t);
    if closed then close_path t
;;

let line_style (t : t) ?width ?color (* ?alignment ?native *) () =
  let color, alpha =
    match color with
    | None -> None, None
    | Some color ->
      let color, alpha = color_to_pixi color in
      Some color, Some alpha
  in
  line_style t.g ?width ?color ?alpha (*  ?alignment ?native *) ()
;;

(* CR-someday: get events of [Container]? *)
let actions t =
  let canvas = (Application.view t.a :> #Html.element Js.t) in
  Dom_wrappers.actions canvas
;;
