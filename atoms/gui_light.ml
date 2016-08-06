open Lwt
open Common
open Dom_wrappers
open Geometry

module L = Lwt_stream

module State = State_light

type t = State.t

let color_for_coordinate =
  let colors =
    [ Color.red; Color.magenta; Color.blue
    ; Color.cyan; Color.green; Color.yellow
    ; Color.red ]
  in
  fun ~width ~height v ->
    let (x, y) = Vector.coords v in
    let color = Color.interpolate colors (y /. height) in
    Color.scale color (x /. width)

let draw_color_picker ctx =
  let width = Ctx.width ctx in
  let height = Ctx.height ctx in
  let cell_size = 3 in
  let cell_size_float = float cell_size in
  for i = 0 to (int width) do
    for j = 0 to (int height) do
      if i mod cell_size = 0 && j mod cell_size = 0
      then begin
        let v = Vector.create i j in
        let color = color_for_coordinate v ~width ~height in
        Ctx.set_fill_color ctx color;
        (* CR: try direct pixel manipulation *)
        Ctx.fill_rect ctx v ~width:cell_size_float ~height:cell_size_float
      end
    done
  done

let run_color_picker t ctx =
  let width = Ctx.width ctx in
  let height = Ctx.height ctx in
  Ctx.canvas_actions ctx
  |> Lwt_stream.iter_with_try ~f:(fun action ->
    let v = Action.coords action in
    let color = color_for_coordinate v ~width ~height in
    let color = Color_cycle.const color in
    State.set_color t color)

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
      let shape = Shape.create ~kind ~frame ~color in
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
  Dom_wrappers.set_reload_on_resize ();
  Random.self_init ();
  let picker_div = get_element_by_id "color-picker-div" Html.CoerceTo.div in
  let picker_ctx =
    Ctx.create ~id:"color-picker-canvas"
      ~width:picker_div##.clientWidth
      ~height:picker_div##.clientHeight
  in
  draw_color_picker picker_ctx;
  let canvas_div = get_element_by_id "main-canvas-div" Html.CoerceTo.div in
  let ctx =
    Ctx.create ~id:"main_canvas"
      ~width:canvas_div##.clientWidth
      ~height:canvas_div##.clientHeight
  in
  let actions = Ctx.canvas_actions ctx in
  choose_shape actions ctx
  >>= fun (shape, action) ->
  State.create ctx shape
  >>= fun t ->
  State.process_action t action;
  Lwt.async (fun () -> run_color_picker t picker_ctx);
  Lwt_stream.iter_with_try actions ~f:(State.process_action t)
