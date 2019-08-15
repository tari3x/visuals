(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

(* Goal is beat identity detection (not touching melody so far).

   Next:
   * normalize volume to perception
   * slowmo
   * Reading: audio source separation (in ocaml)
   * meyda?
*)

(* CR-someday: MFCC?
 * JS-Xtract
 * https://github.com/vail-systems/node-mfcc
 * https://github.com/meyda/meyda/wiki/Audio-Features
 *)

(* Audio feature extraction:

   * google with machine learning:
   https://github.com/google/web-audio-recognition/blob/master/audio-features/src/AudioUtils.ts
   * https://github.com/meyda/meyda
*)

open Base
open Lwt
open Js_of_ocaml
open Js_of_ocaml_lwt
open Common
open Dom_wrappers
open Web_audio
open Typed_array
open Geometry

let source_idle_timeout =
  Time.Span.of_sec 3.

let max_source_age =
  Time.Span.of_sec 30.

let show_spectrum = true

(* CR-someday: how much does zero-allocation matter here? *)

module Source = struct
  module Id = Id(struct let name = "Sound_source_id" end)

  type t =
    { id : Id.t
    ; bin : int
    ; beat : Beat_detector.t
    ; mutable last_in_beat : bool
    ; mutable last_beat_time : Time.t
    ; mutable on_beat : (unit -> unit) list
    ; color : Color.t
    ; created : Time.t
    } [@@deriving fields]

  let do_on_beat t =
    List.iter t.on_beat ~f:(fun f -> f ())

  let create ~beat ~bin =
    let id = Id.create () in
    let color = Color.random () |> Color.maximize in
    let now = Time.now () in
    { id; beat; bin
    ; last_in_beat = false
    ; last_beat_time = now
    ; on_beat = []
    ; color
    ; created = now
    }

  let update t =
    let in_beat = Beat_detector.in_beat t.beat in
    if not t.last_in_beat && in_beat then do_on_beat t;
    t.last_in_beat <- in_beat;
    if in_beat then t.last_beat_time <- Time.now ()

  let in_beat t =
    Beat_detector.in_beat t.beat

  let on_beat t ~f =
    t.on_beat <- f :: t.on_beat

  let is_alive t =
    let open Time in
    let open Span in
    let now = now () in
    let too_idle =
      now - t.last_beat_time > source_idle_timeout
    in
    let too_old =
      now - t.created > max_source_age
    in
    not (too_idle || too_old)
end

module Event = struct
  type t =
  | Beat   of Source.t
  | Delete of Source.t
end

type t =
  { analyser : AnalyserNode.t
  ; bins : uint8Array Js.t
  ; beats : Beat_detector.t array
  ; max_sources : int
  ; mutable sources : Source.t list
  ; mutable volume : float
  ; mutable on_event : (Event.t -> unit) list
  ; mutable debug : Ctx.t option
  ; mutable started : bool
  }

let num_bins t =
  t.bins##.length

let bin_volume_exn t i =
  float (Typed_array.unsafe_get t.bins i) /. 256.

let do_on_beat t ~source =
  List.iter t.on_event ~f:(fun f -> f (Beat source))

let find_new_source t ~sources =
  if List.length sources >= t.max_sources then None
  else if List.exists sources ~f:Source.in_beat then None
  else match Array.count t.beats ~f:Beat_detector.in_beat with
  | 0 -> None
  | num_beats ->
    let n = ref (Random.int num_beats + 1) in
    let (bin, beat) =
      Array.findi_exn t.beats ~f:(fun _i beat ->
        if Beat_detector.in_beat beat then n := !n - 1;
        !n = 0)
    in
    let source = Source.create ~beat ~bin in
    Source.on_beat source ~f:(fun () -> do_on_beat t ~source);
    Some source

let delete_source_event t source =
  List.iter t.on_event ~f:(fun f -> f (Delete source))

let update_sources t =
  List.iter t.sources ~f:Source.update;
  let sources, deleted_sources = List.partition_tf t.sources ~f:Source.is_alive in
  List.iter deleted_sources ~f:(delete_source_event t);
  let sources = Option.to_list (find_new_source t ~sources) @ sources in
  t.sources <- sources

let draw t ctx =
  let open Float in
  Ctx.clear ctx;
  Ctx.save ctx;
  Ctx.set_fill_color ctx Color.white;
  Ctx.set_line_width ctx 5.;
  (* let min_freq = Freq.of_hertz 0. in *)
  let max_freq = Freq.of_hertz (t.analyser##.context##.sampleRate / 2.) in
  let _num_visible_bins = num_bins t in
  let ctx_w = Ctx.width ctx in
  let ctx_h = Ctx.height ctx in
  let volume_w = 100. in
  let empty_w = 100. in
  let spectrum_w = ctx_w - volume_w - empty_w in
  let freq_bin_width =
    Freq.hertz max_freq / float (num_bins t)
  in
  let bin_w i =
    let i = float i in
    let left  = Freq.of_hertz ( i       * freq_bin_width) |> Freq.mel in
    let right = Freq.of_hertz ((i + 1.) * freq_bin_width) |> Freq.mel in
    (right - left) * spectrum_w / Freq.mel max_freq
    |> Float.max 1.
  in
  let cursor = ref 0. in
  let draw_bin ~value ~width ?ewma ?short_ewma () =
    let height = value * ctx_h * 0.1 in
    let v = Vector.create_float !cursor (ctx_h - height) in
    Ctx.fill_rect ctx v ~width ~height;
    let draw_line ~color value_height =
      Ctx.set_stroke_color ctx color;
      let height = value_height * ctx_h in
      let v1 = Vector.create_float !cursor (ctx_h - height) in
      let v2 = Vector.(v1 + (create_float width 0.)) in
      Ctx.begin_path ctx;
      Ctx.move_to ctx v1;
      Ctx.line_to ctx v2;
      Ctx.stroke ctx;
    in
    Option.iter ewma ~f:(draw_line ~color:Color.green);
    Option.iter short_ewma ~f:(draw_line ~color:Color.yellow);
    cursor := !cursor + width
  in
  Array.iteri t.beats ~f:(fun i beat ->
    let color =
      List.find t.sources ~f:(fun source ->
        Source.in_beat source && Int.(Source.bin source = i))
      |> function
        | Some source -> Source.color source
        | None ->
          if not show_spectrum
          then Color.black
          else if Beat_detector.in_beat beat
          then Color.red
          else Color.white
    in
    Ctx.set_fill_color ctx color;
    let value = bin_volume_exn t i in
    let width = bin_w i in
    let ewma = Beat_detector.Debug.ewma beat |> Option.value ~default:0.5 in
    let short_ewma = Beat_detector.Debug.short_ewma beat |> Option.value ~default:0.5 in
    let short_ewma = if short_ewma > ewma then Some short_ewma else None in
    draw_bin ~value ~width ~ewma ?short_ewma ()
  );
  draw_bin ~value:0. ~width:empty_w ();
  draw_bin ~value:t.volume ~width:volume_w ();
  Ctx.restore ctx

let draw t =
  Option.iter t.debug ~f:(draw t)

let rec update_loop (t : t) =
  (*
  Lwt_js_events.request_animation_frame ()
  >>= fun () ->
  *)
  Lwt_js.sleep 0.01
  >>= fun () ->
  t.analyser##getByteFrequencyData t.bins;
  let time = Time.now () in
  Array.iteri t.beats ~f:(fun i beat ->
    let value = bin_volume_exn t i in
    Beat_detector.add_sample beat ~time ~value);
  update_sources t;
  let last_bin = num_bins t in
  let volume =
    t.bins##subarray 0 last_bin
    |> Typed_array.fold ~init:0 ~f:Int.(+)
    |> float
    |> Float.scale (1. /. float last_bin)
    |> Float.scale (1. /. 256.)
  in
  t.volume <- volume;
  draw t;
  update_loop t

let create_from_src ~ctx ~src ~max_sources =
  let analyser = ctx##createAnalyser () in
  analyser##.smoothingTimeConstant := 0.; (* 0.85 *)
  analyser##.minDecibels := (-90.);
  analyser##.maxDecibels := (-10.);
  analyser##.fftSize := 4096;
  src##connect ~destination:(analyser :> AudioNode.t);
  analyser##connect ~destination:(ctx##.destination :> AudioNode.t);
  (* 1024 by default *)
  let num_bins = analyser##.frequencyBinCount in
  let bins = new%js uint8Array num_bins in
  let beats = Array.init num_bins ~f:(fun _i -> Beat_detector.create ()) in
  { analyser
  ; bins
  ; beats
  ; max_sources
  ; sources = []
  ; volume = 0.
  ; on_event = []
  ; debug = None
  ; started = false
  }

let start t =
  if not t.started
  then Lwt.async (fun () -> update_loop t);
  t.started <- true

let create_from_html ~id ~max_sources =
  let audio = Audio.create ~id in
  let ctx = AudioContext.create () in
  let src = ctx##createMediaElementSource audio in
  create_from_src ~ctx ~src ~max_sources

let create_from_mic ~max_sources =
  let open Js_std in
  let constraints =
    MediaStreamConstraints.of_js_expr "{audio : true, video : false}"
  in
  Lwt.wrap (fun c -> get_user_media_exn c ~constraints)
  >>= fun stream ->
  let ctx = AudioContext.create () in
  let src = ctx##createMediaStreamSource stream in
  create_from_src ~ctx ~src ~max_sources
  |> Lwt.return

let on_event t ~f =
  t.on_event <- f :: t.on_event

let set_debug t ctx =
  t.debug <- ctx
