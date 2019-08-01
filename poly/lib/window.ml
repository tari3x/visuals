(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Core
open Async
open Std_internal

let debug a = debug_s ~enabled:false a

module G = Graphics
module L = Lagrange

let degree = 8

let min_points = 15
let max_points = 15 (* 18 *)

let num_frames = 90_000

let speed = 0.2
(*
  let speed = Float.(0.15 / 8.5)
*)

let config =
  Config.create
    ~grid_size:(10, 10)
    ~cbrange:(-10., 10.)
    ~image_width:1024
    ~rendering_degree:degree
    ~speed
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
    ; poly : P.t
    ; prev : t
    } [@@deriving fields]

  module Update = struct
    type t =
    | Add_zero of V.t
    | Move_zero of V.t
    | Drop_zero
    | Move_nonzero of V.t
    | Rotate_zeroes
    | Undo
    | Finish
        [@@deriving sexp]

    module Animation = struct
      type t =
      | None
      | Move_point_by of V.t
      | Interpolate_poly
    end

    let animation_vector state t : Animation.t =
      match t with
      | Undo
      | Finish -> None
      | Drop_zero
      | Add_zero _
      | Rotate_zeroes -> Interpolate_poly
      | Move_nonzero v ->
        Move_point_by V.(v - state.nonzero)
      | Move_zero v ->
        match state.zeroes with
        | [] -> None
        | v' :: _ ->
          Move_point_by V.(v - v')
  end

  let active_zero t =
    List.hd t.zeroes

  let make_lagrange_base ~zeroes ~nonzero =
    let zeroes =
      match zeroes with
      | [] -> []
      | _ :: zeroes ->
        List.map zeroes ~f:(fun v -> v, 0.)
    in
    let data = (nonzero, 100.) :: zeroes in
    let basis =
      P.Basis.bernstein ~domain:(Config.domain config) ~degree
      |> P.Basis.odd_powers_only
    in
    L.create ~basis data

  let make_poly ~zeroes ~lagrange_base =
    let lagrange =
      match zeroes with
      | [] -> lagrange_base
      | v :: _ -> Lagrange.add lagrange_base ~data:[ v, 0. ]
    in
    Lagrange.result lagrange

  let make_poly ~zeroes ~lagrange_base =
    Probe.with_probe "poly" (fun () -> make_poly ~zeroes ~lagrange_base)

  let create ?(zeroes = []) () =
    let nonzero = Config.domain_centre config in
    let lagrange_base = make_lagrange_base ~zeroes ~nonzero in
    let poly = make_poly ~zeroes ~lagrange_base in
    let rec t =
      { zeroes
      ; nonzero
      ; lagrange_base
      ; poly
      ; prev = t
      }
    in
    t

  let update t (update : Update.t) =
    let
        { zeroes
        ; nonzero
        ; lagrange_base
        ; poly = _
        ; prev
        } = t
    in
    match update with
    | Finish -> t
    | Undo -> prev
    | Add_zero v ->
      let zeroes = v :: zeroes in
      let lagrange_base = make_lagrange_base ~zeroes ~nonzero in
      let poly = make_poly ~zeroes ~lagrange_base in
      { zeroes
      ; nonzero
      ; lagrange_base
      ; poly
      ; prev = t
      }
    | Drop_zero ->
      begin match zeroes with
      | [] -> t
      | _ :: zeroes ->
        let lagrange_base = make_lagrange_base ~zeroes ~nonzero in
        let poly = make_poly ~zeroes ~lagrange_base in
        { zeroes
        ; nonzero
        ; lagrange_base
        ; poly
        ; prev = t
        }
      end
    | Move_zero v ->
      let zeroes =
        match zeroes with
        | [] -> [ v ]
        | _ :: vs -> v :: vs
      in
      let poly = make_poly ~zeroes ~lagrange_base in
      { zeroes
      ; nonzero
      ; lagrange_base
      ; poly
      ; prev
      }
    | Move_nonzero v ->
      let lagrange_base = make_lagrange_base ~zeroes ~nonzero in
      let poly = make_poly ~zeroes ~lagrange_base in
      { zeroes
      ; nonzero = v
      ; lagrange_base
      ; poly
      ; prev
      }
    | Rotate_zeroes ->
      match List.rev zeroes with
      | [] -> t
      | v :: vs ->
        let zeroes = v :: List.rev vs in
        let lagrange_base = make_lagrange_base ~zeroes ~nonzero in
        let poly = make_poly ~zeroes ~lagrange_base in
        { zeroes; nonzero; lagrange_base; poly; prev = t }

  let _draw_dots t =
    let draw_dot v =
      let (i, j) = Config.domain_to_image config v in
      G.fill_circle i j 10;
    in
    match t.zeroes with
    | [] -> ()
    | v :: vs ->
      G.set_color G.red;
      draw_dot v;
      G.set_color G.white;
      List.iter vs ~f:draw_dot


  (* CR-someday: this is only correct if only the last zero has moved. *)
  let point_weighted_average ~w t1 t2 =
    let v_avg = V.weighted_average ~w in
    let { zeroes = zs1; nonzero = nz1; lagrange_base = _
        ; prev = _; poly = _ } = t1 in
    let { zeroes = zs2; nonzero = nz2; lagrange_base; prev; poly = _ } = t2 in
    let zeroes = List.map2_exn zs1 zs2 ~f:v_avg in
    let nonzero = v_avg nz1 nz2 in
    let poly = make_poly ~zeroes ~lagrange_base in
    { zeroes; nonzero; lagrange_base; prev; poly }

  (* CR-someday: this produces pretty fake states. *)
  let poly_weighted_average ~w t1 t2 =
    let poly = P.weighted_average t1.poly t2.poly ~w in
    { t2 with poly }

  (* CR: optimize. *)
  let distance t1 t2 =
    let image_x, image_y = Config.image_size config in
    let icon_x = 64 in
    let icon_y = 64 in
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

module type Events = sig
  type t

  val create : unit -> t

  val next : t -> State.t -> State.Update.t option
end

module User_events : Events = struct
  type t = { mutable key : char }

  let create () =
    { key = Char.of_int_exn 0 }

  let next t _state : State.Update.t option =
    let event = G.wait_next_event [ Button_down; Key_pressed ] in
    debug [%message (event : Event.t)];
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

let random_point () =
  let open Float in
  let (x, y) = Config.domain_size config in
  let c_x = x / 2. in
  let c_y = y / 2. in
  let w = 0.8 in
  let w_x = x * w in
  let w_y = y * w in
  let x1 = c_x - w_x / 2. in
  let y1 = c_y - w_y / 2. in
  (x1 + Random.float w_x, y1 + Random.float w_y)

let random_point_not_too_close v d =
  let count = ref 0 in
  let rec loop () =
    incr count;
    if !count > 1000
    then failwith "too many loops of random point";
    let v' = random_point () in
    if Float.(V.(length (v - v')) >= d)
    then v'
    else loop ()
  in
  loop ()

module Auto_events : Events = struct
  module Q = Queue

  type t =
    { updates : State.Update.t Q.t
    ; mutable count : int
    ; mutable next_zero_action : [ `add | `remove ]
    }

  let create () =
    { updates = Q.create ()
    ; count = 0
    ; next_zero_action = `remove
    }

  let next_zero_action t (state : State.t) : State.Update.t =
    Core.print_s [%message "Adding or removing zero"];
    let action =
      let len = List.length state.zeroes in
      if len = min_points
      then `add
      else if len = max_points
      then `remove
      else t.next_zero_action
    in
    t.next_zero_action <- action;
    match action with
    | `add -> Add_zero (random_point ())
    | `remove -> Drop_zero

  let next (t : t) state : State.Update.t option =
    if Q.is_empty t.updates
    then begin
      t.count <- t.count + 1;
      if t.count % 3 = 0
      then Q.enqueue t.updates (next_zero_action t state)
      else Q.enqueue t.updates Rotate_zeroes;
      match State.active_zero state with
      | None -> ()
      | Some v ->
        let v = random_point_not_too_close v 4. in
        Q.enqueue t.updates (Move_zero v);
    end;
    Some (Q.dequeue_exn t.updates)
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
    ; palettes : Palette.Basis.t Pipe.Reader.t
    ; mutable num_frames : int
    }

  let create ~record =
    let eval = Eval.Ctx.create ~config in
    let palettes = Palette.Basis.pipe config in
    let recording =
      if record then Some (Animation.create ~config []) else None
    in
    { eval; recording; palettes; num_frames = 0 }

  let record_frame t palette p =
    t.num_frames <- t.num_frames + 1;
    t.recording <- Option.map t.recording ~f:(fun recording ->
      let state =
        Animation.State.of_poly p
        |> Animation.State.with_palette ~palette
      in
      { recording with states = state :: recording.states })

  let write_animation_exn t file =
    match t.recording with
    | None -> failwith "did not record recording"
    | Some recording ->
      let recording = { recording with states = List.rev recording.states } in
      Writer.save_sexp file (Animation.sexp_of_t recording)

  let next_palette t =
    match%bind Pipe.read t.palettes with
    | `Eof -> assert false
    | `Ok basis -> return  basis
end

let initial_state ~num_points =
  let zeroes = List.init num_points ~f:(fun _ -> random_point ()) in
  State.create ~zeroes ()

let draw_poly (ctx : Ctx.t) palette p ~write_to =
  Ctx.record_frame ctx palette p;
  let values = Probe.with_probe "eval" (fun () -> Eval.values ctx.eval p) in
  match write_to with
  | None ->
    Probe.start "palette";
    let palette = Palette.create_graphics palette in
    Probe.stop "palette";
    Probe.start "colors";
    for i = 0 to A2.dim1 values - 1 do
      for j = 0 to A2.dim2 values - 1 do
        let color =
          A2.get_flipped values i j
          |> Array.get palette
        in
        colors.(j).(i) <- color;
      done
    done;
    Probe.stop "colors";
    let image = Probe.with_probe "image" (fun () -> G.make_image colors) in
    Probe.with_probe "draw" (fun () -> G.draw_image image 0 0)
  | Some dir ->
    Probe.start "palette";
    let palette = Palette.create palette in
    Probe.stop "palette";
    Render_camlimage.write_image ~dir ~config ~values ~palette ()

let run ~record_to ~write_to =
  let module Events = Auto_events in
  Random.self_init ();
  let ctx = Ctx.create ~record:(Option.is_some record_to) in
  let events = Events.create () in
  (* 0.02, 0.05, 0.2 *)
  let max_distance = Float.(0.1 * Config.speed config) in
  let draw ~stop prev_state state =
    let%bind palette = Ctx.next_palette ctx in
    Or_error.try_with (fun () ->
      State.poly state |> draw_poly ctx palette ~write_to)
    |> Or_error.iter_error ~f:(Core.eprintf !"%{Error#hum}\n%!");
    (* State.draw_dots state; *)
    (* G.draw_string (Float.to_string distance); *)
    begin
      match prev_state with
      | None -> ()
      | Some prev_state ->
        let _distance = State.distance prev_state state in
        let _stop_threshold = Float.(max_distance / 30.) in
        if ctx.num_frames >= num_frames then stop ();
	(*
	 if Float.(distance < stop_threshold)
	 then begin
	 Core.print_s [%message "STOP"];
	 stop ()
	 end
	 *)
    end;
    return (Some state)
  in
  let rec loop states =
    let stop () = Pipe.close_read states in
    let%bind state = Pipe.fold states ~init:None ~f:(draw ~stop) in
    Core.print_s [%message "next event"];
    let state = Option.value_exn state ~here:[%here] in
    (* Probe.print_and_clear (); *)
    match Events.next events state with
    | None -> loop (Pipe.singleton state)
    | Some Finish -> return ()
    | Some update ->
      debug [%message (update : State.Update.t)];
      let new_state = State.update state update in
      let states =
        match State.Update.animation_vector state update with
        | None -> Pipe.singleton new_state
        | Interpolate_poly ->
          Motion.smooth_speed
            ~point:(fun w -> State.poly_weighted_average state new_state ~w)
            ~distance:State.distance
            ~max_distance
            ~desired_step_size:1.
        | Move_point_by v ->
          let absolute_desired_step_size = Float.(0.2 * Config.speed config) in
          let desired_step_size = absolute_desired_step_size /. (V.length v) in
          Motion.smooth_speed
            ~point:(fun w -> State.point_weighted_average state new_state ~w)
            ~distance:State.distance
            ~max_distance
            ~desired_step_size
      in
      if ctx.num_frames >= num_frames
      then return ()
      else loop states
  in
  let image_x, image_y = Config.image_size config in
  G.open_graph (sprintf " %dx%d" image_x image_y);
  G.set_font "-adobe-courier-*-*-*-*-*-240-*-*-*-*-*-*";
  let%bind () = loop (Pipe.singleton (initial_state ~num_points:max_points)) in
  match record_to with
  | None -> return ()
  | Some file -> Ctx.write_animation_exn ctx file

let command =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
     let record_to = flag "-record-to" (optional Filename.arg_type) ~doc:""
     and write_to = flag "-write-to" (optional Filename.arg_type) ~doc:"" in
     fun () ->
       run ~record_to ~write_to
    ]
