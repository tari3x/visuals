(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Core
open Async
open Std_internal

let debug a = debug_s ~enabled:true a

module G = Graphics
module L = Lagrange

let degree = 4

let config =
  Config.create
    ~grid_size:(10, 10)
    ~cbrange:(-10., 10.)
    ~image_width:1024
    ~rendering_degree:degree
    ()

module Event = struct
  type t = G.status =
    { mouse_x : int
    ; mouse_y : int
    ; button : bool
    ; keypressed : bool
    ; key : char
    } [@@deriving sexp]
end

module State = struct
  type t =
    { zeroes : V.t list
    ; nonzero : V.t
    (* all but the last zero *)
    ; lagrange_base : L.t
    ; prev : t
    }

  module Update = struct
    type t =
    | Add_zero of V.t
    | Move_zero of V.t
    | Move_nonzero of V.t
    | Rotate_zeroes
    | Undo
    | Finish
        [@@deriving sexp]

    let should_animate = function
      | Add_zero _
      | Undo
      | Rotate_zeroes
      | Finish -> false
      | Move_zero _
      | Move_nonzero _ -> true
  end

  let lagrange ~zeroes ~nonzero =
   let zeroes = List.map zeroes ~f:(fun v -> v, 0.) in
   let data = (nonzero, 100.) :: zeroes in
   let basis = P.Basis.mono ~degree in
   L.create ~basis data

  let create () =
    let nonzero = Config.domain_centre config in
    let zeroes = [] in
    let lagrange_base = lagrange ~zeroes ~nonzero in
    let rec t =
      { zeroes = []
      ; nonzero
      ; lagrange_base
      ; prev = t
      }
    in
    t

  let update t (update : Update.t) =
    let
        { zeroes
        ; nonzero
        ; lagrange_base = _
        ; prev
        } = t
    in
    match update with
    | Finish -> t
    | Undo -> prev
    | Add_zero v ->
      let lagrange_base = lagrange ~zeroes ~nonzero in
      { zeroes = v :: zeroes
      ; nonzero
      ; lagrange_base
      ; prev = t
      }
    | Move_zero v ->
      let zeroes =
        match List.rev zeroes with
        | [] -> [ v ]
        | _:: vs -> List.rev (v :: vs)
      in
      let lagrange_base = lagrange ~zeroes ~nonzero in
      { zeroes
      ; nonzero
      ; lagrange_base
      ; prev
      }
      (*
      let zeroes =
        match zeroes with
        | [] -> [ v ]
        | _ :: vs -> v :: vs
      in
      { zeroes
      ; nonzero
      ; lagrange_base
      ; prev
      }
      *)
    | Move_nonzero v ->
      let old_zeroes = List.tl zeroes |> Option.value ~default:[] in
      let lagrange_base = lagrange ~zeroes:old_zeroes ~nonzero in
      { zeroes
      ; nonzero = v
      ; lagrange_base
      ; prev
      }
    | Rotate_zeroes ->
      match List.rev zeroes with
      | [] -> t
      | v :: vs ->
        let zeroes = v :: List.rev vs in
        let lagrange_base = lagrange ~zeroes ~nonzero in
        { zeroes; nonzero; lagrange_base; prev = t }

  let poly { zeroes; nonzero = _; lagrange_base; prev = _ } =
    let lagrange =
      match zeroes with
      | [] -> lagrange_base
      | v :: _ -> Lagrange.add lagrange_base ~data:[ v, 0. ]
    in
    Lagrange.result lagrange

  let poly t =
    Probe.with_probe "poly" (fun () -> poly t)

  let _draw_dots { zeroes; lagrange_base = _; nonzero = _; prev = _ } =
    G.set_color G.white;
    List.iter zeroes ~f:(fun v ->
      let (i, j) = Config.domain_to_image config v in
      G.draw_circle i j 10)

  (* CR-someday: this is only correct if only the last zero has moved. *)
  let weighted_average ~w t1 t2 =
    let v_avg = V.weighted_average ~w in
    let { zeroes = zs1; nonzero = nz1; lagrange_base = _; prev = _ } = t1 in
    let { zeroes = zs2; nonzero = nz2; lagrange_base = _; prev } = t2 in
    let zeroes = List.map2_exn zs1 zs2 ~f:v_avg in
    let nonzero = v_avg nz1 nz2 in
    let lagrange_base = lagrange ~zeroes ~nonzero in
    { zeroes; nonzero; lagrange_base; prev }

  (* CR: optimize. *)
  let _distance t1 t2 =
    let image_x, image_y = Config.image_size config in
    let icon_x = 8 in
    let icon_y = 8 in
    let x_ratio = image_x / icon_x in
    let y_ratio = image_y / icon_y in
    let p1 = poly t1 in
    let p2 = poly t2 in
    let count = ref 0 in
    for i = 0 to icon_x - 1 do
      for j = 0 to icon_y - 1 do
        let i = i * x_ratio in
        let j = j * y_ratio in
        let x, y = Config.image_to_domain config (i, j) in
        let v1 = P.eval_point p1 (x, y) in
        let v2 = P.eval_point p2 (x, y) in
        if Float.(v1 * v2 < 0.) then incr count;
      done
    done;
    Float.(float !count / (float icon_x * float icon_y))
end

module Events = struct
  type t = { mutable key : char }

  let create () =
    { key = Char.of_int_exn 0 }

  let feed_event t event : State.Update.t option =
    let { G.
      mouse_x
    ; mouse_y
    ; button
    ; keypressed
    ; key
    } = event
    in
    let v = Config.image_to_domain config (mouse_x, mouse_y) in
    let mouse_event () : State.Update.t option =
      if not button then None
      else match t.key with
      | 'z' -> Some (Add_zero v)
      | 'm' -> Some (Move_zero v)
      | 'n' -> Some (Move_nonzero v)
      | _ -> None
    in
    if keypressed
    then begin
      t.key <- key;
      match key with
      | 'u' -> Some Undo
      | 'f' -> Some Finish
      | 'r' -> Some Rotate_zeroes
      | _ -> mouse_event ()
    end
    else mouse_event ()
end

let colors =
  let image_x, image_y = Config.image_size config in
  Array.init image_y ~f:(fun _ ->
    Array.init image_x ~f:(fun _ ->
       Graphics.rgb 0 0 0))

module Ctx = struct
  type t =
    { eval : Eval.Ctx.t
    ; mutable recording : Animation.t option
    }

  let create ~record =
    let eval = Eval.Ctx.create ~config in
    let recording =
      if record then Some (Animation.create ~config []) else None
    in
    { eval; recording }

  let record_frame t p =
    t.recording <- Option.map t.recording ~f:(fun recording ->
      let state = Animation.State.of_poly p in
      { recording with states = state :: recording.states })

  let write_animation_exn t file =
    match t.recording with
    | None -> failwith "did not record recording"
    | Some recording ->
      let recording = { recording with states = List.rev recording.states } in
      Writer.save_sexp file (Animation.sexp_of_t recording)
end

let draw_poly (ctx : Ctx.t) p =
  Ctx.record_frame ctx p;
  let values = Probe.with_probe "eval" (fun () -> Eval.values ctx.eval p) in
  Probe.start "colors";
  for i = 0 to A2.dim1 values - 1 do
    for j = 0 to A2.dim2 values - 1 do
      let color =
        A2.get_flipped values i j
        |> Render.value_graphics_color
      in
      colors.(j).(i) <- color;
    done
  done;
  Probe.stop "colors";
  let image = Probe.with_probe "image" (fun () -> G.make_image colors) in
  Probe.with_probe "draw" (fun () -> G.draw_image image 0 0)

let run ~record_to =
  let ctx = Ctx.create ~record:(Option.is_some record_to) in
  let events = Events.create () in
  let draw state =
    Or_error.try_with (fun () ->
      State.poly state |> draw_poly ctx)
    |> Or_error.iter_error ~f:(Core.eprintf !"%{Error#hum}\n%!");
    (* State.draw_dots state; *)
    state
  in
  let rec loop states =
    let%bind states = Pipe.map states ~f:draw |> Pipe.to_list in
    let state = List.last_exn states in
    Probe.print_and_clear ();
    let event = G.wait_next_event [ Button_down; Key_pressed ] in
    debug [%message (event : Event.t)];
    match Events.feed_event events event with
    | None -> loop (Pipe.singleton state)
    | Some Finish -> return ()
    | Some update ->
      debug [%message (update : State.Update.t)];
      let new_state = State.update state update in
      let states =
        if State.Update.should_animate update
        then
          interpolate
            ~weighted_average:State.weighted_average
            ~num_steps:20
            [ state; new_state ]
          |> Pipe.of_list
          (*
          Motion.smooth_speed
            ~point:(fun w -> State.weighted_average state new_state ~w)
            ~distance:State.distance
            ~max_distance:0.02 (* 0.05, 0.2 *)
            ~desired_step_size:0.02 (* 0.05, 0.2 *)
          *)
        else Pipe.singleton new_state
      in
      loop states
  in
  let image_x, image_y = Config.image_size config in
  G.open_graph (sprintf " %dx%d" image_x image_y);
  let%bind () = loop (Pipe.singleton (State.create ())) in
  match record_to with
  | None -> return ()
  | Some file -> Ctx.write_animation_exn ctx file

let command =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
     let record_to = flag "-record-to" (optional Filename.arg_type) ~doc:"" in
     fun () ->
       run ~record_to
    ]
