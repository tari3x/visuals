(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Js
open Typed_array
open Js_std

module BaseAudioContext : sig
  type witness
  class type js = object
    method base_audio_context_witness : witness
    method sampleRate : float readonly_prop
  end

  type t = js Js.t
end

module AudioNode : sig
  type witness
  class type js = object
    method audio_node_witness : witness
    method connect : destination:js Js.t -> unit meth
    method context : BaseAudioContext.t readonly_prop
  end

  type t = js Js.t
end

module MediaStreamAudioSourceNode : sig
  type witness
  class type js = object
    inherit AudioNode.js
    method media_stream_audio_source_node_witness : witness
  end

  type t = js Js.t
end

module MediaStreamAudioDestinationNode : sig
  type witness
  class type js = object
    inherit AudioNode.js
    method media_stream_audio_destination_node_witness : witness
  end

  type t = js Js.t
end

module AnalyserNode : sig
  type witness
  class type js = object
    inherit AudioNode.js
    method analyser_node_witness : witness
    (* Must be a power of 2 between 2^5 and 2^15, *)
    method fftSize : int prop
    method frequencyBinCount : int readonly_prop
    (* From -infty to 0. WTF am I supposed to do with it? *)
    method getFloatFrequencyData : float32Array Js.t -> unit meth
    method getByteFrequencyData : uint8Array Js.t -> unit meth
    (* between 0 and 1 *)
    method smoothingTimeConstant : float prop
    method minDecibels : float prop
    method maxDecibels : float prop
  end

  type t = js Js.t
end

(* CR-someday: what's with this:
   var audioCtx = new (window.AudioContext || window.webkitAudioContext)();
*)
module AudioContext : sig
  type witness
  class type js = object
    inherit BaseAudioContext.js
    method audio_context_witness : witness
    method createAnalyser : unit -> AnalyserNode.t meth
    method createMediaElementSource
      :  Dom_html.audioElement Js.t
      -> MediaStreamAudioSourceNode.t meth
    method createMediaStreamSource
      :  MediaStream.t
      -> MediaStreamAudioSourceNode.t meth
    (* CR-someday: actually part of [BaseAudioContext] but creates a cyclic
       dependency. *)
    method destination : MediaStreamAudioDestinationNode.t readonly_prop
  end

  type t = js Js.t

  val create : unit -> t
end
