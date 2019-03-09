open Core
open Std_internal

let debug a = debug ~enabled:true a

module G = Graphics

let degree = 1

let config =
  Config.create
    ~grid_size:(10, 10)
    ~cbrange:(-10., 10.)
    ~image_width:1024
    ()

module Event = struct
  type t = G.status =
    {
      mouse_x : int;
      mouse_y : int;
      button : bool;
      keypressed : bool;
      key : char;
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
    let basis = P.Basis.Kind.mono ~degree in
    P.lagrange ~basis data

  let draw_dots { zeroes; nonzero = _; prev = _ } =
    G.set_color G.white;
    List.iter zeroes ~f:(fun v ->
      let (i, j) = Config.domain_to_image config v in
      G.draw_circle i j 10)
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

let draw_poly ctx p =
  debug "poly: %s" (P.to_string p);
  let values = P.values ctx p in
  let image =
    Array.init (A2.dim1 values) ~f:(fun j ->
      Array.init (A2.dim2 values) ~f:(fun i ->
        let p_value = A2.get_flipped values i j in
        Render.value_color ~config p_value
        |> Color.graphics_color))
    |> G.make_image
  in
  G.draw_image image 0 0

let run () =
  let ctx = P.Eval_ctx.create ~config ~degree in
  let events = Events.create () in
  let draw state =
    Or_error.try_with (fun () ->
      State.poly state |> draw_poly ctx)
    |> Or_error.iter_error ~f:(Core.eprintf !"%{Error#hum}\n%!");
    State.draw_dots state
  in
  let rec loop state =
    draw state;
    let event = G.wait_next_event [ Button_down; Key_pressed ] in
    debug !"event: %{sexp:Event.t}" event;
    match Events.feed_event events event with
    | None -> loop state
    | Some update ->
      debug !"update: %{sexp:State.Update.t}" update;
      let state = State.update state update in
      loop state
  in
  let image_x, image_y = Config.image_size config in
  G.open_graph (sprintf " %dx%d" image_x image_y);
  loop (State.create ())
