open Core
open Std_internal

let debug a = debug ~enabled:false a

module G = Graphics

let degree = 2

let config =
  Config.create
    ~grid_size:(10, 10)
    ~cbrange:(-10., 10.)
    ~image_width:1024
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
    ; prev : t
    }

  module Update = struct
    type t =
    | Add_zero of V.t
    | Move_zero of V.t
    | Move_nonzero of V.t
    | Undo
        [@@deriving sexp]

    let should_animate = function
      | Add_zero _
      | Undo -> false
      | Move_zero _
      | Move_nonzero _ -> true
  end

  let create () =
    let rec t =
      { zeroes = []
      ; nonzero = Config.domain_centre config
      ; prev = t
      }
    in
    t

  let update t (update : Update.t) =
    let
        { zeroes
        ; nonzero
        ; prev
        } = t
    in
    match update with
    | Undo -> prev
    | Add_zero v ->
      { zeroes = v :: zeroes
      ; nonzero
      ; prev = t
      }
    | Move_zero v ->
      let zeroes =
        match zeroes with
        | [] -> [ v ]
        | _ :: vs -> v :: vs
      in
      { zeroes
      ; nonzero
      ; prev
      }
    | Move_nonzero v ->
      { zeroes
      ; nonzero = v
      ; prev
      }

  let poly { zeroes; nonzero; prev = _ } =
    let zeroes = List.map zeroes ~f:(fun v -> v, 0.) in
    let data =  zeroes @ [ nonzero, 100. ] in
    (*
    let basis =
      P.Basis.Kind.bernstein ~degree ~domain:(Config.domain config)
    in
    *)
    let basis = P.Basis.mono ~degree in
    Lagrange.simple ~basis data

  let poly t =
    Probe.with_probe "poly" (fun () -> poly t)

  let draw_dots { zeroes; nonzero = _; prev = _ } =
    G.set_color G.white;
    List.iter zeroes ~f:(fun v ->
      let (i, j) = Config.domain_to_image config v in
      G.draw_circle i j 10)

  let weighted_average ~w t1 t2 =
    let v_avg = V.weighted_average ~w in
    let { zeroes = zs1; nonzero = nz1; prev = _ } = t1 in
    let { zeroes = zs2; nonzero = nz2; prev } = t2 in
    let zeroes = List.map2_exn zs1 zs2 ~f:v_avg in
    let nonzero = v_avg nz1 nz2 in
    { zeroes; nonzero; prev }
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
    }

  let create () =
    let eval = Eval.Ctx.create ~config ~degree in
    { eval }
end

let draw_poly (ctx : Ctx.t) p =
  debug "poly: %s" (P.to_string p);
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

let run () =
  let ctx = Ctx.create () in
  let events = Events.create () in
  let draw state =
    Or_error.try_with (fun () ->
      State.poly state |> draw_poly ctx)
    |> Or_error.iter_error ~f:(Core.eprintf !"%{Error#hum}\n%!");
    State.draw_dots state
  in
  let rec loop states =
    List.iter states ~f:draw;
    let state = List.last_exn states in
    Probe.print_and_clear ();
    let event = G.wait_next_event [ Button_down; Key_pressed ] in
    debug !"event: %{sexp:Event.t}" event;
    match Events.feed_event events event with
    | None -> loop [state]
    | Some update ->
      debug !"update: %{sexp:State.Update.t}" update;
      let new_state = State.update state update in
      let states =
        if State.Update.should_animate update
        then interpolate [ state; new_state ]
          ~num_steps:20
          ~weighted_average:State.weighted_average
        else [new_state]
      in
      loop states
  in
  let image_x, image_y = Config.image_size config in
  G.open_graph (sprintf " %dx%d" image_x image_y);
  loop [State.create ()]
