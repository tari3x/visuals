(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

module MediaStreamConstraints : sig
  type witness
  class type js = object
    method media_stream_constraints_witness : witness
  end

  type t = js Js.t

  (* Examples:
     { audio: true, video: true }

     {
     audio: true,
     video: { width: 1280, height: 720 }
     }
  *)
  val of_js_expr : string -> t
end

module MediaStream : sig
  type witness
  class type js = object
    method media_stream_witness : witness
  end

  type t = js Js.t
end

val get_user_media_exn
  :  constraints:MediaStreamConstraints.t
  -> (MediaStream.t -> unit)
  -> unit

val read_camera : Dom_html.videoElement Js.t -> unit
