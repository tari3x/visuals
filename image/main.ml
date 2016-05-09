open Lwt
open Js

module Html = Dom_html

let error f = Printf.ksprintf (fun s -> Firebug.console##(error (Js.string s)); failwith s) f
let debug f = Printf.ksprintf (fun s -> Firebug.console##(log (Js.string s))) f
let alert f = Printf.ksprintf (fun s -> Html.window##(alert (Js.string s)); failwith s) f

let float = float_of_int
let int   = int_of_float

let pi = 4.0 *. atan 1.0

let lwt_wrap f =
  let (t, w) = Lwt.task () in
  let cont x = Lwt.wakeup w x in
  f cont;
  t

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
  ctx##arc (Point.x p) (Point.y p) radius 0. (2. *. pi) _false;
  ctx##fill;
  (* CR: is this necessary? *)
  ctx##beginPath

let setup_canvas id =
  let canvas = get_element_by_id id Html.CoerceTo.canvas in
  let ctx = canvas##getContext Html._2d_ in
  canvas##.width := Html.document##.body##.clientWidth;
  canvas##.height := Html.document##.body##.clientHeight;
  canvas, ctx

let setup_spotlight () =
  let canvas, ctx = setup_canvas "spotlight_canvas" in
  ctx##.fillStyle := (string "white");
  let active = ref false in
  add_event_listener canvas Html.Event.mousedown ~f:(fun ev ->
    active := true);
  add_event_listener canvas Html.Event.mouseup ~f:(fun ev ->
    clear ctx;
    active := false);
  add_event_listener canvas Html.Event.mousemove ~f:(fun ev ->
    if !active then begin
      let x = ev##.clientX and y = ev##.clientY in
      let p = Point.create x y in
      clear ctx;
      plot_point ctx p 10.
    end
  )

let main () =
  let canvas, ctx = setup_canvas "image_canvas" in
  load_image "image.png"
  >>= fun image ->
  ctx##drawImage_withSize image
    0. 0.
    (float canvas##.clientWidth) (float canvas##.clientHeight);
  setup_spotlight ();
  Lwt.return ()

let go _ = ignore (
  catch (fun () -> main ())
    (fun exn -> error "uncaught exception: %s" (Printexc.to_string exn)));
  _true

;;

Html.window##.onload := Html.handler go
