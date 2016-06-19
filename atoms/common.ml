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

module Option = struct
  type 'a t = 'a option
  let iter t ~f =
    match t with
    | None -> ()
    | Some x -> f x
end

module List = struct
  include ListLabels

  let delete xs x =
    filter xs ~f:(fun x' -> x' <> x)

  let bring_to_front xs x =
    x :: delete xs x

  let maybe_find t ~f =
    try Some (find t ~f) with
    | Not_found -> None
end

module Hashtbl = struct
  include Hashtbl

  let create () =
    create 1000

  let iter t ~f = iter f t

  let maybe_find t key =
    try Some (find t key)
    with Not_found -> None
end

(* CR: there is Identifiable *)
module type Id = sig
  type t
  val create : unit -> t
end

module Id = struct
  type t = int

  let create () =
    Random.int 100_000_000
end

module Client_id : Id = Id

(* Must be collision-free *)
module Shape_id : Id = Id

module Color = struct
  type t =
    { r : int
    ; g : int
    ; b : int
    ; a : float
    }

  let wave ~cycle time =
    ((sin (time *. cycle) +. 1.) /. 2.) *. 255. |> int

  let of_time time =
    let r = wave ~cycle:1. time in
    let g = wave ~cycle:2. time in
    let b = wave ~cycle:3. time in
    let a = 0.7 in
    { r; g; b; a }

  let to_string { r; g; b; a } =
    Printf.sprintf "rgba(%d, %d, %d, %f)" r g b a

  let random () =
    let r = Random.int 255 in
    let g = Random.int 255 in
    let b = Random.int 255 in
    let a = 1. in
    { r; g; b; a }
end

module Fn = struct
  let flip f x y =
    f y x
end

module Point : sig
  type t

  val create : int -> int -> t
  val coords : t -> (float * float)
  val x : t -> float
  val y : t -> float

  val (-) : t -> t -> t
  val (+) : t -> t -> t

  val to_string : t -> string
end = struct
  type t = (float * float * float)

  let create_float x y =
    (x, y, 1.)

  let create x y =
    create_float (float x) (float y)

  let coords (x, y, _) =
    (x, y)

  let x (x, _, _) = x
  let y (_, y, _) = y

  let (-) t1 t2 =
    let x = x t1 -. x t2 in
    let y = y t1 -. y t2 in
    create_float x y

  let (+) t1 t2 =
    let x = x t1 +. x t2 in
    let y = y t1 +. y t2 in
    create_float x y

  let to_string (x, y, _) =
    Printf.sprintf "(%f, %f)" x y
end

module Button = struct
  type t = [ `left | `right | `middle | `touch | `none ]
end

module Mouse_event = struct
  type t = Dom_html.mouseEvent Js.t

  let client_coords t =
    Point.create t##.clientX t##.clientY

  let button t : Button.t =
    match Html.buttonPressed t with
    | Html.No_button     -> `none
    | Html.Left_button   -> `left
    | Html.Middle_button -> `middle
    | Html.Right_button  -> `right
end

(* CR: make it work with multi-touch. *)
module Touch_event = struct
  type t = Dom_html.touchEvent Js.t

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

module Ctx = struct
  type t = Html.canvasRenderingContext2D Js.t

  let clear t =
    let width = t##.canvas##.clientWidth |> float in
    let height = t##.canvas##.clientHeight |> float in
    t##clearRect 0. 0. width height

  let plot_point t p radius =
    t##beginPath;
    t##arc (Point.x p) (Point.y p) radius 0. (2. *. Js.math##._PI) _false;
    t##fill;
    (* CR: is this necessary? *)
    t##beginPath

  let draw_dot t p radius =
    plot_point t p radius

  let draw_horizontal_line (t : t) p width =
    let y = Point.y p in
    let top = y -. width /. 2. in
    let screen_width = float (t##.canvas##.clientWidth) in
    t##fillRect 0. top screen_width width

  let draw_vertical_line (t : t) p width =
    let x = Point.x p in
    let left = x -. width /. 2. in
    let screen_height = float (t##.canvas##.clientHeight) in
    t##fillRect left 0. width screen_height

  let draw_rectangle (t : t) p ~width ~height =
    let x = Point.x p in
    let y = Point.y p in
    t##fillRect x y width height

  let set_fill_color (t : t) color =
    t##.fillStyle := string (Color.to_string color)
end

