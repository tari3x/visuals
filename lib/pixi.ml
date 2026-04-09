open Js_of_ocaml
open Common
open Geometry
open Dom_wrappers

open (
  Pixi_lib : module type of Pixi_lib with module Color := Pixi_lib.Color)

module A = Application
module C = Container

module Matrix = struct
  include Pixi_lib.Matrix

  let create m =
    let get = Geometry.Matrix.get m in
    let t = create () in
    (*     t##set (get 0 0) (get 1 0) (get 0 1) (get 1 1) (get 0 2) (get 1 2); *)
    [| get 0 0; get 0 1; get 0 2; get 1 0; get 1 1; get 1 2 |]
    (* [| get 0 0; get 1 0; get 0 2; get 0 1; get 1 1; get 1 2 |] *)
    (* [| get 0 0; get 0 1; get 2 0; get 1 0; get 1 1; get 2 1 |] *)
    (* [| get 0 0; get 1 0; get 0 1; get 1 1; get 0 2; get 1 2 |] *)
    (* [| get 0 0; get 0 1; get 0 2; get 1 0; get 1 1; get 1 2 |] *)
    |> from_array t;
    t
  ;;
end

(* CR-someday avatar: this looks like an allocator *)
let color_to_pixi color =
  let r, g, b, alpha = Color.components color in
  Pixi_lib.Color.create r g b, alpha
;;

module Graphics = struct
  include Graphics

  let fill (t : t) color =
    let color, alpha = color_to_pixi color in
    fill t ~color ~alpha ()
  ;;

  let stroke (t : t) color ~width =
    let color, alpha = color_to_pixi color in
    stroke t ~color ~alpha ~width ()
  ;;

  let move_to t v =
    let x = Vector.x v in
    let y = Vector.y v in
    move_to t x y
  ;;

  let line_to t v =
    let x = Vector.x v in
    let y = Vector.y v in
    line_to t x y
  ;;

  let path (t : t) ~closed vs =
    match vs with
    | [] -> ()
    | v :: vs ->
      move_to t v;
      List.iter vs ~f:(line_to t);
      if closed then close_path t
  ;;

  let set_tint_and_alpha (t : t) color =
    let t = (t :> Container.t) in
    let color, alpha = color_to_pixi color in
    Container.set_tint t color;
    Container.set_alpha t alpha
  ;;

  let set_visible (t : t) visible =
    let t = (t :> Container.t) in
    Container.set_visible t (Js.bool visible)
  ;;

  let set_zindex (t : t) zindex =
    let t = (t :> Container.t) in
    Container.set_zindex t zindex
  ;;

  let set_transform t m =
    reset_transform t;
    transform t m
  ;;
end

type t = A.t

let init_exn () =
  let open Lwt.Let_syntax in
  let a = A.create () in
  let%bind () = A.init a |> Promise_lwt.of_promise in
  let (_ : Dom.node Js.t) =
    Html.document##.body##appendChild (A.view a :> Dom.node Js.t)
  in
  (* CR-someday avatar: this creates scrollbars, but I hide them with overflow:
     hidden

     I think you might need innerWidth and innerHeight!
  *)
  A.resize_to a Dom_html.window;
  A.resize a;
  Lwt.return a
;;

let create_graphics t =
  let g = Graphics.create () in
  C.add_child (A.stage t) (g :> DisplayObject.t);
  g
;;

let width t = Canvas.width (A.view t)
let height t = Canvas.height (A.view t)

(* CR-someday: get events of [Container]? *)
let actions t =
  let canvas = (Application.view t :> #Html.element Js.t) in
  Dom_wrappers.actions canvas
;;
