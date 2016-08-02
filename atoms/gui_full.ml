open Lwt
open Common
open Dom_wrappers

module State = State_full

type t = State.t

let add_canvas_handlers t canvas =
  add_event_listener canvas Html.Event.mousedown ~f:(fun ev ->
    State.process_action t (Mouse_event.action ev `down));
  add_event_listener canvas Html.Event.mouseup ~f:(fun ev ->
    State.process_action t (Mouse_event.action ev `up));
  add_event_listener canvas Html.Event.mousemove ~f:(fun ev ->
    State.process_action t (Mouse_event.action ev `move));
  add_event_listener canvas Html.Event.touchstart ~f:(fun ev ->
    Dom.preventDefault ev;
    State.process_action t (Touch_event.action ev `down));
  add_event_listener canvas Html.Event.touchend ~f:(fun ev ->
    Dom.preventDefault ev;
    State.process_action t (Touch_event.action ev `up));
  add_event_listener canvas Html.Event.touchmove ~f:(fun ev ->
    Dom.preventDefault ev;
    State.process_action t (Touch_event.action ev `move))

let range_max id =
  let input = get_element_by_id id Html.CoerceTo.input in
  Js.Unsafe.get input "max" |> Js.to_string |> float_of_string

let get_color_cycle color_cycle =
  let set_color id color : unit =
    Js.Unsafe.eval_string
      (Printf.sprintf "$('#%s').spectrum('set', '%s')" id (Color.to_string color))
  in
  let set_value id value =
    let input = get_element_by_id id Html.CoerceTo.input in
    input##.value := Js.string (string_of_int (int_of_float value))
  in
  let { Color_cycle. colors = _; length; offset } = color_cycle in
  let max_offset = range_max "cycle-offset" in
  set_value "cycle-length" length;
  set_value "cycle-offset" (offset *. max_offset);
  set_color "color1" (Color_cycle.nth_defaulting_to_last_or_white color_cycle 0);
  set_color "color2" (Color_cycle.nth_defaulting_to_last_or_white color_cycle 1);
  set_color "color3" (Color_cycle.nth_defaulting_to_last_or_white color_cycle 2);
  set_color "color4" (Color_cycle.nth_defaulting_to_last_or_white color_cycle 3)

let set_color_cycle t =
  let get_color id =
    let button = get_element_by_id id Html.CoerceTo.input in
    button##.value |> Js.to_string |> Color.of_hex8_string
  in
  let range_value id =
    let input = get_element_by_id id Html.CoerceTo.input in
    input##.value |> Js.to_string |> float_of_string
  in
  let length = range_value "cycle-length" in
  let offset = range_value "cycle-offset" in
  let max_offset = range_max "cycle-offset" in
  let color1 = get_color "color1" in
  let color2 = get_color "color2" in
  let color3 = get_color "color3" in
  let color4 = get_color "color4" in
  let color =
    { Color_cycle.
      colors = [ color1; color2; color3; color4 ]
    ; length
    ; offset = offset /. max_offset
    }
  in
  State.set_color t color

let get_simple_color color_cycle =
  let set_color id color =
    let input = get_element_by_id id Html.CoerceTo.input in
    input##.value := Js.string (string_of_int (int_of_float color))
  in
  let max_alpha = range_max "alpha" in
  let color = Color_cycle.nth_defaulting_to_last_or_white color_cycle 0 in
  set_color "red"   (Color.r color |> float_of_int);
  set_color "green" (Color.g color |> float_of_int);
  set_color "blue"  (Color.b color |> float_of_int);
  set_color "alpha" (Color.a color *. max_alpha)

let set_simple_color t =
  let get_color id =
    let input = get_element_by_id id Html.CoerceTo.input in
    input##.value |> Js.to_string |> float_of_string
  in
  let max_alpha = range_max "alpha" in
  let color =
    Color.create
      ~r:(get_color "red"  |> int_of_float)
      ~g:(get_color "green"|> int_of_float)
      ~b:(get_color "blue" |> int_of_float)
      ~a:(get_color "alpha" /. max_alpha)
  in
  let color = Color_cycle.const color in
  State.set_color t color

let add_cycle_handlers t =
  (* let get_button = get_element_by_id "get-color" Html.CoerceTo.button in *)
  let set_button = get_element_by_id "set-color" Html.CoerceTo.button in
  let get_button = get_element_by_id "get-color" Html.CoerceTo.button in
  add_event_listener set_button Html.Event.touchstart ~f:(fun _ ->
    set_color_cycle t);
  add_event_listener set_button Html.Event.click ~f:(fun _ ->
    set_color_cycle t);
  add_event_listener get_button Html.Event.touchstart ~f:(fun _ ->
    Option.iter (State.most_recent t) ~f:(fun shape ->
      get_color_cycle (Shape.color shape)));
  add_event_listener get_button Html.Event.click ~f:(fun _ ->
    Option.iter (State.most_recent t) ~f:(fun shape ->
      get_color_cycle (Shape.color shape)))

let add_picker_handlers t =
  List.iter [ "red"; "green"; "blue"; "alpha" ] ~f:(fun id ->
    let input = get_element_by_id id Html.CoerceTo.input in
    add_event_listener input Html.Event.touchmove ~f:(fun _ ->
      set_simple_color t);
    add_event_listener input Html.Event.mousemove ~f:(fun _ ->
      set_simple_color t));
  State.on_shape_active t ~f:(fun shape ->
    get_simple_color (Shape.color shape))

let add_choice_handlers t =
  let add_listener id ~f =
    let elt = get_element_by_id id Html.CoerceTo.button in
    add_event_listener elt Html.Event.touchstart ~f:(fun _ -> f ())
  in
  add_listener "R" ~f:(fun () -> State.set_shape_kind t Shape.Kind.Rectangle);
  add_listener "C" ~f:(fun () -> State.set_shape_kind t Shape.Kind.Circle);
  add_listener "H" ~f:(fun () -> State.set_shape_kind t Shape.Kind.Horizontal_line);
  add_listener "V" ~f:(fun () -> State.set_shape_kind t Shape.Kind.Vertical_line);
  add_listener "X" ~f:(fun () -> State.set_shape_kind t Shape.Kind.Cross_line);
  add_listener "T" ~f:(fun () -> State.toggle_transient_mode t)

let add_toolbar_handlers t =
  add_cycle_handlers t;
  add_picker_handlers t;
  add_choice_handlers t

let main ~is_server =
  Random.self_init ();
  let canvas = get_element_by_id "main_canvas" Html.CoerceTo.canvas in
  canvas##.width := Html.document##.body##.clientWidth;
  canvas##.height := Html.document##.body##.clientHeight;
  let ctx = canvas##getContext Html._2d_ in
  State.create ctx ~is_server
  >>= fun t ->
  add_canvas_handlers t canvas;
  if not is_server then add_toolbar_handlers t;
  Lwt.return ()
;;
