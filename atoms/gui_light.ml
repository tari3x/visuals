open Lwt
open Common
open Dom_wrappers
open Geometry

module L = Lwt_stream

module State = State_light

type t = State.t

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
      let shape = Shape.create ~kind ~frame ~color ~line_width:10. () in
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
  let shape =
    List.nth shapes n
    |> Shape.set ~line_width:Shape.default_line_width
  in
  Lwt.return (shape, action)

let main () =
  Dom_wrappers.set_reload_on_resize ();
  Random.self_init ();
  let picker_div = get_element_by_id "color-picker-div" Html.CoerceTo.div in
  let picker_ctx =
    Ctx.create ~id:"color-picker-canvas"
      ~width:picker_div##.clientWidth
      ~height:picker_div##.clientHeight
  in
  Color_picker.draw picker_ctx;
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
  Lwt.async (fun () -> Color_picker.run t picker_ctx);
  Lwt_stream.iter_with_try actions ~f:(State.process_action t)
