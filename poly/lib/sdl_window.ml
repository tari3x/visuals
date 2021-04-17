open Core
open Result
open Result.Let_syntax
open Tsdl
open Tgl4

let gl = 4, 3

let pp_opengl_info ppf () =
  let pp = Format.fprintf in
  let pp_opt ppf = function
    | None -> pp ppf "error"
    | Some s -> pp ppf "%s" s
  in
  pp ppf "@[<v>@,";
  pp ppf "Renderer @[<v>@[%a@]@," pp_opt (Gl.get_string Gl.renderer);
  pp
    ppf
    "@[OpenGL %a / GLSL %a@]@]@,"
    pp_opt
    (Gl.get_string Gl.version)
    pp_opt
    (Gl.get_string Gl.shading_language_version);
  pp ppf "@]"
;;

type t =
  { win : Sdl.window
  ; ctx : Sdl.gl_context
  ; event : Sdl.event
  ; w : int
  ; h : int
  }

let create ~w ~h =
  let maj, min = gl in
  Sdl.init Sdl.Init.video
  >>= fun () ->
  let w_atts = Sdl.Window.(opengl + resizable) in
  let w_title = Printf.sprintf "OpenGL %d.%d (core profile)" maj min in
  let set a v = Sdl.gl_set_attribute a v in
  set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core
  >>= fun () ->
  set Sdl.Gl.context_major_version maj
  >>= fun () ->
  set Sdl.Gl.context_minor_version min
  >>= fun () ->
  set Sdl.Gl.doublebuffer 1
  >>= fun () ->
  Sdl.create_window ~w ~h w_title w_atts
  >>= fun win ->
  Sdl.gl_create_context win
  >>= fun ctx ->
  Sdl.gl_make_current win ctx
  >>= fun () ->
  Sdl.log "%a" pp_opengl_info ();
  Gl.viewport 0 0 w h;
  let event = Sdl.Event.create () in
  Ok { win; ctx; w; h; event }
;;

let swap_buffers { win; _ } = Sdl.gl_swap_window win
let reshape w h = Gl.viewport 0 0 w h

module Event = struct
  type t =
    | Quit
    | Exposed of
        { w : int
        ; h : int
        }
    | Resized of
        { w : int
        ; h : int
        }
end

let poll_event { win; event; _ } : Event.t option =
  let key_scancode e =
    Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode)
  in
  let window_event e =
    Sdl.Event.(window_event_enum (get e window_event_id))
  in
  if not (Sdl.poll_event (Some event))
  then None
  else (
    let w, h = Sdl.get_window_size win in
    match Sdl.Event.(enum (get event typ)) with
    | `Quit -> Some Quit
    | `Key_down when Poly.(key_scancode event = `Escape) -> None
    | `Window_event ->
      (match window_event event with
      | `Exposed ->
        reshape w h;
        Some (Exposed { w; h })
      | `Resized ->
        reshape w h;
        Some (Resized { w; h })
      | _ -> None)
    | _ -> None)
;;

let check_events t =
  match poll_event t with
  | None -> ()
  | Some e ->
    (match e with
    | Quit -> exit 0
    | Exposed _ | Resized _ -> ())
;;
