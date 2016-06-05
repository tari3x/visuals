open Lwt
open Js

module Html = Dom_html

let error f = Printf.ksprintf (fun s -> Firebug.console##(error (Js.string s)); failwith s) f
let debug f = Printf.ksprintf (fun s -> Firebug.console##(log (Js.string s))) f
let alert f = Printf.ksprintf (fun s -> Html.window##(alert (Js.string s)); failwith s) f

let float = float_of_int
let int   = int_of_float

let lwt_wrap f =
  let (t, w) = Lwt.task () in
  let cont x = Lwt.wakeup w x in
  f cont;
  t

module Fn = struct
  let flip f x y =
    f y x
end

module Point : sig
  type t

  val create : int -> int -> t
  val x : t -> float
  val y : t -> float
end = struct
  type t = (float * float * float)

  let create x y =
    (float x, float y, 1.)

  let x (x, _, _) = x
  let y (_, y, _) = y
end

module Mouse_event = struct
  let client_coords ev =
    Point.create ev##.clientX ev##.clientY
end

(* CR: make it work with multi-touch. *)
module Touch_event = struct
  type t =
    Dom_html.touchEvent Js.t

  let client_coords (ev : t) =
    let touches = ev##.touches in
    let touch =
      touches##item 0
      |> Fn.flip Optdef.get (fun () ->
        failwith "Touch_event.client_coords")
    in
    Point.create touch##.clientX touch##.clientY
end

let add_event_listener elt event ~f =
  Html.addEventListener elt event
    (Html.handler
       (fun ev ->
         f ev;
         Js._true))
    Js._true
  |> ignore

let get_element_by_id id coerce_to =
  Opt.get
    (Opt.bind ( Html.document##getElementById(string id) )
       coerce_to)
    (fun () -> error "can't find element %s" id)

let load_image src =
  let img = Html.createImg Html.document in
  lwt_wrap
    (fun c ->
      img##.onload := Html.handler (fun _ -> c (); Js._false);
      img##.src := (string src))
  >>= fun () ->
  Lwt.return img

let clear ctx =
  let width = ctx##.canvas##.clientWidth |> float in
  let height = ctx##.canvas##.clientHeight |> float in
  ctx##clearRect 0. 0. width height

let plot_point ctx p radius =
  ctx##beginPath;
  ctx##arc (Point.x p) (Point.y p) radius 0. (2. *. Js.math##._PI) _false;
  ctx##fill;
  (* CR: is this necessary? *)
  ctx##beginPath

let draw_dot ctx p radius =
  plot_point ctx p radius

let draw_horizontal_line
    (ctx : Html.canvasRenderingContext2D Js.t)
    (point : Point.t)
    (width : float)
    () =
  let y = Point.y point in
  let top = y -. width /. 2. in
  let screen_width = float (ctx##.canvas##.clientWidth) in
  ctx##fillRect 0. top screen_width width

let draw_vertical_line
    (ctx : Html.canvasRenderingContext2D Js.t)
    (point : Point.t)
    (width : float)
    () =
  let x = Point.x point in
  let left = x -. width /. 2. in
  let screen_height = float (ctx##.canvas##.clientHeight) in
  ctx##fillRect left 0. width screen_height
