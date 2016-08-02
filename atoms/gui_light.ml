open Lwt
open Common
open Dom_wrappers
open Geometry

module L = Lwt_stream

module State = State_light

type t = State.t

(*
let set_color t color =
  let red_slider = get_element_by_id "red-slider" Html.CoerceTo.canvas in
  let green_slider = get_element_by_id "green-slider" Html.CoerceTo.canvas in
  let blue_slider = get_element_by_id "blue-slider" Html.CoerceTo.canvas in
  let set_slider_
*)

let set_color t =
  let get_color id =
    let input = get_element_by_id id Html.CoerceTo.input in
    input##.value |> Js.to_string |> float_of_string
  in
  let color =
    Color.create
      ~r:(get_color "red-slider"  |> int_of_float)
      ~g:(get_color "green-slider"|> int_of_float)
      ~b:(get_color "blue-slider" |> int_of_float)
      ~a:1.
  in
  let color = Color_cycle.const color in
  State.set_color t color

let add_picker_handlers t =
  List.iter [ "red-slider"; "green-slider"; "blue-slider" ] ~f:(fun id ->
    let input = get_element_by_id id Html.CoerceTo.input in
    add_event_listener input Html.Event.touchmove ~f:(fun _ ->
      set_color t);
    add_event_listener input Html.Event.mousemove ~f:(fun _ ->
      set_color t))

let mousedown_action_and_coords elt =
  Lwt_js_events.mousedown elt
  >>= fun ev ->
  Dom.preventDefault ev;
  let action = Mouse_event.action ev `down in
  let p = Mouse_event.client_coords ev in
  return (action, p)

let touchstart_action_and_coords elt =
  Lwt_js_events.touchstart elt
  >>= fun ev ->
  Dom.preventDefault ev;
  let action = Touch_event.action ev `down in
  let p = Action.coords action in
  return (action, p)

let choose_shape (actions : Action.t Lwt_stream.t) ctx =
  (* CR: do better estimate of the toolbar width. *)
  let width = Ctx.width ctx |> int_of_float in
  let height = Ctx.height ctx |> int_of_float in
  let hstep = width / List.length Shape.Kind.examples in
  let color = Color_cycle.random_constant () in
  let shapes =
    List.mapi Shape.Kind.examples ~f:(fun i kind ->
      let x = hstep * i + hstep / 2 in
      let y = height / 2 in
      let frame = Frame.translate (Vector.create x y) in
      let shape =
        Shape.create ~kind ~frame ~color
      in
      let margin = min 50 (hstep / 5) in
      let clip_size = min hstep height - margin in
      let clip_p =
        Vector.create
          (hstep * i + (margin / 2))
          ((height - clip_size) / 2)
      in
      Ctx.save ctx;
      Ctx.clip_rect ctx clip_p ~width:clip_size ~height:clip_size;
      let shape = Shape.scale_to_fit shape (float_of_int clip_size) in
      (* CR: need late time *)
      Shape.render shape ctx ~time:100.;
      Ctx.restore ctx;
      shape
    )
  in
  Lwt_stream.find actions ~f:(fun action -> action.kind = `down)
  >>= fun action ->
  let p = Action.coords action in
  let n = int_of_float (Vector.x p) / hstep in
  Ctx.clear ctx;
  Lwt.return (List.nth shapes n, action)

let main () =
  Random.self_init ();
  let canvas_div = get_element_by_id "main-canvas-div" Html.CoerceTo.div in
  let canvas = get_element_by_id "main_canvas" Html.CoerceTo.canvas in
  (* CR: why the fuck is this necessary? Setting values via CSS fucks things
     up *)
  canvas##.width := canvas_div##.clientWidth;
  canvas##.height := canvas_div##.clientHeight;
  let ctx = canvas##getContext Html._2d_ in
  let actions = Dom_wrappers.actions canvas in
  choose_shape actions ctx
  >>= fun (shape, action) ->
  State.create ctx shape
  >>= fun t ->
  State.process_action t action;
  add_picker_handlers t;
  Lwt_stream.iter_with_try actions ~f:(State.process_action t)