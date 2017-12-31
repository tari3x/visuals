(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Js

module Object = struct
  type witness
  class type js = object
    method object_witness : witness
    method toString : js_string Js.t meth
  end

(*
  type t = js Js.t
*)
end

module MediaStreamConstraints = struct
  type witness
  class type js = object
    method media_stream_constraints_witness : witness
  end

  type t = js Js.t

  let of_js_expr e =
    Js.Unsafe.eval_string (Printf.sprintf "var x = %s; x" e)
end

module MediaStream = struct
  type witness
  class type js = object
    method media_stream_witness : witness
  end

  type t = js Js.t
end

module MediaStreamError = struct
  type witness
  class type js = object
    inherit Object.js
    method media_stream_error_witness : witness
  end

  type t = js Js.t
end

external get_user_media
  :  constraints:MediaStreamConstraints.t
  -> on_success:(MediaStream.t -> unit) callback
    -> on_error:(MediaStreamError.t -> unit) callback
      -> unit
        = "js_get_user_media"

let get_user_media_exn ~constraints f =
  let on_error (error : MediaStreamError.t) =
    failwith (Js.to_string error##toString)
  in
  get_user_media
    ~constraints
    ~on_success:(Js.wrap_callback f)
    ~on_error:(Js.wrap_callback on_error)

external read_camera : Dom_html.videoElement Js.t -> unit = "js_read_camera"
