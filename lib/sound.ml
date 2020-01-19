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

   * latency
     https://askubuntu.com/questions/491825/pulseaudio-loopback-latency
     http://juho.tykkala.fi/Pulseaudio-and-latency
*)

(* CR-someday:

   * MFCC?
   * JS-Xtract
   * https://github.com/vail-systems/node-mfcc
   * https://github.com/meyda/meyda/wiki/Audio-Features
   * http://practicalcryptography.com/miscellaneous/machine-learning/guide-mel-frequency-cepstral-coefficients-mfccs/

   Audio feature extraction:
   -----------------------

   * google with machine learning:
   https://github.com/google/web-audio-recognition/blob/master/audio-features/src/AudioUtils.ts
   * https://github.com/meyda/meyda
*)

(* TODO:

   * use width in mel not just for drawing
   * cut off the very high end of the spectrum (MFCC does that)
   * cut off the very low end of the spectrum?
*)

open Base
open Lwt
open Js_of_ocaml
open Js_of_ocaml_lwt
open Common_lib
open Common
open Dom_wrappers
open Web_audio
open Typed_array
open Geometry

let smoothing_window = 5.
let source_idle_timeout = Time.Span.of_sec 3.
let max_source_age = Time.Span.of_sec 30.
let show_spectrum = true
let show_beats = true
let max_freq = Freq.of_hertz 8000.
let wave_window = 0.2

(* CR-someday: how much does zero-allocation matter here? *)

module BD = Beat_detector
module MA = Moving_average

module Source = struct
  module Id = Id (struct
    let name = "Sound_source_id"
  end)

  type t =
    { id : Id.t
    ; bin : int
    ; beat : BD.t
    ; mutable last_in_beat : bool
    ; mutable last_beat_time : Time.t
    ; mutable on_beat : (unit -> unit) list
    ; color : Color.t
    ; created : Time.t
    }
  [@@deriving fields]

  let do_on_beat t = List.iter t.on_beat ~f:(fun f -> f ())

  let create ~beat ~bin =
    let id = Id.create () in
    let color = Color.random () |> Color.maximize in
    let now = Time.now () in
    { id
    ; beat
    ; bin
    ; last_in_beat = false
    ; last_beat_time = now
    ; on_beat = []
    ; color
    ; created = now
    }
  ;;

  let update t =
    let in_beat = BD.in_beat t.beat in
    if (not t.last_in_beat) && in_beat then do_on_beat t;
    t.last_in_beat <- in_beat;
    if in_beat then t.last_beat_time <- Time.now ()
  ;;

  let in_beat t = BD.in_beat t.beat
  let on_beat t ~f = t.on_beat <- f :: t.on_beat

  let is_alive t =
    let open Time in
    let open Span in
    let now = now () in
    let too_idle = now - t.last_beat_time > source_idle_timeout in
    let too_old = now - t.created > max_source_age in
    not (too_idle || too_old)
  ;;
end

module Event = struct
  type t =
    | Beat of Source.t
    | Delete of Source.t
    | Wave of float
end

type t =
  { analyser : AnalyserNode.t
  ; fft_bins : uint8Array Js.t
  ; bins : BD.t array
  ; max_sources : int
  ; (* CR-someday: normalizer for this as well? *)
    wave : MA.t
  ; mutable sources : Source.t list
  ; mutable on_event : (Event.t -> unit) list
  ; mutable debug : Ctx.t option
  ; mutable started : bool
  }

let num_bins t = Array.length t.bins

let bin_volume_exn t i =
  float (Typed_array.unsafe_get t.fft_bins i) /. 256.
;;

let do_on_beat t ~source =
  let id = Source.id source |> Source.Id.hash in
  match List.length t.on_event with
  | 0 -> ()
  | n ->
    let n = id % n in
    let f = List.nth_exn t.on_event n in
    f (Beat source)
;;

(* List.iter t.on_event ~f:(fun f -> f (Beat source)) *)

let find_new_source t ~sources =
  if List.length sources >= t.max_sources
  then None
  else if List.exists sources ~f:Source.in_beat
  then None
  else (
    match Array.count t.bins ~f:BD.in_beat with
    | 0 -> None
    | num_bins ->
      let n = ref (Random.int num_bins + 1) in
      let bin, beat =
        Array.findi_exn t.bins ~f:(fun _i beat ->
            if BD.in_beat beat then n := !n - 1;
            !n = 0)
      in
      let source = Source.create ~beat ~bin in
      Source.on_beat source ~f:(fun () -> do_on_beat t ~source);
      Some source)
;;

let delete_source_event t source =
  List.iter t.on_event ~f:(fun f -> f (Delete source))
;;

let update_sources t =
  List.iter t.sources ~f:Source.update;
  let sources, deleted_sources =
    List.partition_tf t.sources ~f:Source.is_alive
  in
  List.iter deleted_sources ~f:(delete_source_event t);
  let sources = Option.to_list (find_new_source t ~sources) @ sources in
  t.sources <- sources
;;

let draw t ctx =
  let open Float in
  Ctx.clear ctx;
  Ctx.save ctx;
  Ctx.set_fill_color ctx Color.white;
  Ctx.set_line_width ctx 5.;
  let _min_freq = Freq.of_hertz 44. in
  (* let freq_width = max_freq - min_freq in *)
  let ctx_w = Ctx.width ctx in
  let ctx_h = Ctx.height ctx in
  let wave_w = 100. in
  let empty_w = 100. in
  let spectrum_w = ctx_w - wave_w - empty_w in
  let freq_bin_width = Freq.hertz max_freq / float (num_bins t) in
  (* CR-someday: make an array of freqs to make more efficient. *)
  let bin_w i =
    (* spectrum_w / float num_visible_bins *)
    let i = float i in
    let left = Freq.of_hertz (i * freq_bin_width) |> Freq.hertz in
    let right = Freq.of_hertz ((i + 1.) * freq_bin_width) |> Freq.hertz in
    (right - left) * spectrum_w / Freq.hertz max_freq |> Float.max 1.
  in
  let cursor = ref 0. in
  let draw_bin ~value ~width ?long ?wave ?beat ?beat_min () =
    let height = value * ctx_h in
    let v = Vector.create_float !cursor (ctx_h - height) in
    Ctx.fill_rect ctx v ~width ~height;
    let draw_line ~color value_height =
      Ctx.set_stroke_color ctx color;
      let height = value_height * ctx_h * 0.2 in
      let v1 = Vector.create_float !cursor (ctx_h - height) in
      let v2 = Vector.(v1 + create_float width 0.) in
      Ctx.begin_path ctx;
      Ctx.move_to ctx v1;
      Ctx.line_to ctx v2;
      Ctx.stroke ctx
    in
    Option.iter long ~f:(draw_line ~color:Color.green);
    Option.iter wave ~f:(draw_line ~color:Color.yellow);
    Option.iter beat ~f:(draw_line ~color:Color.red);
    Option.iter beat_min ~f:(draw_line ~color:Color.blue);
    cursor := !cursor + width
  in
  Array.iteri t.bins ~f:(fun i beat ->
      (* let value = bin_volume_exn t i in *)
      let value, color =
        List.find t.sources ~f:(fun source ->
            Source.in_beat source && Int.(Source.bin source = i))
        |> function
        | Some source when show_beats -> 0.3, Source.color source
        | _ ->
          ( 0.2
          , if not show_spectrum
            then Color.black
            else (
              match BD.signal beat with
              | None -> Color.white
              | Some Beat -> Color.red
              | Some (Wave wave) -> Color.set_alpha Color.green ~alpha:wave)
          )
      in
      Ctx.set_fill_color ctx color;
      let width = bin_w i in
      let long = BD.Debug.long beat in
      let wave = BD.Debug.wave beat in
      let beat_min = None (* BD.Debug.beat_min beat *) in
      let beat = None (*  BD.Debug.beat beat *) in
      (* let wave = if wave > long then Some wave else None in *)
      draw_bin ~value ~width ?long ?wave ?beat ?beat_min ());
  draw_bin ~value:0. ~width:empty_w ();
  Ctx.set_fill_color ctx Color.yellow;
  let wave = MA.get_exn t.wave in
  draw_bin ~value:wave ~width:wave_w ();
  Ctx.restore ctx
;;

let draw t = Option.iter t.debug ~f:(draw t)

let rec update_loop (t : t) =
  let open Float in
  (*
  Lwt_js_events.request_animation_frame ()
  >>= fun () ->
  *)
  (* CR-someday: to much work with lower loop interval *)
  let loop_interval = 0.01 in
  Lwt_js.sleep loop_interval
  >>= fun () ->
  t.analyser##getByteFrequencyData t.fft_bins;
  let time = Time.now () in
  let param = Time.to_sec time in
  let avg = MA.create ~window:smoothing_window in
  Array.iteri t.bins ~f:(fun i beat ->
      let value = bin_volume_exn t i in
      MA.add avg ~param:(Float.of_int i) ~value;
      let value = MA.get_exn avg in
      BD.add beat ~time ~value);
  let wave =
    Array.fold t.bins ~init:0. ~f:(fun acc bin ->
        acc + (BD.wave bin |> Option.value ~default:0.))
    / float (num_bins t)
  in
  MA.add t.wave ~param ~value:wave;
  List.iter t.on_event ~f:(fun f ->
      f (Wave (5. * loop_interval * MA.get_exn t.wave)));
  update_sources t;
  draw t;
  update_loop t
;;

(**
   See https://webaudio.github.io/web-audio-api/#dictdef-analyseroptions
*)
let create_from_src ~ctx ~src ~max_sources =
  let analyser = ctx##createAnalyser () in
  (* Setting this to 0. makes the values ultra sensitive, so even for waves they
     keep coming back to 0, and the min filter doesn't work.

     Perhaps I should doe smoothing myself, but it is applied before converting
     to dB in the analyzer, so it wouldn't be the same.  *)
  (* analyser##.smoothingTimeConstant := 0.3; *)
  (* from -90. to -10. *)
  analyser##.minDecibels := -100.;
  analyser##.maxDecibels := -50.;
  (* Default is 2048, but the spectrum looks a lot better behaved with lower
     values, there's no weird spike at the low frequencies. At least for the
     monitor, for the microphone it is still very bottom-heavy. *)
  analyser##.fftSize := 256;
  src##connect ~destination:(analyser :> AudioNode.t);
  analyser##connect ~destination:(ctx##.destination :> AudioNode.t);
  (* frequencyBinCount is half the fftSize. *)
  let num_fft_bins = analyser##.frequencyBinCount in
  let fft_bins = new%js uint8Array num_fft_bins in
  let num_bins =
    let open Float in
    float num_fft_bins * (Freq.hertz max_freq / 24_000.) |> Int.of_float
  in
  let bins = Array.init num_bins ~f:(fun _i -> BD.create ()) in
  let wave = MA.create ~window:wave_window in
  { analyser
  ; fft_bins
  ; bins
  ; max_sources
  ; sources = []
  ; on_event = []
  ; debug = None
  ; started = false
  ; wave
  }
;;

let start t =
  if not t.started then Lwt.async (fun () -> update_loop t);
  t.started <- true
;;

let create_from_html ~id ~max_sources =
  let audio = Audio.create ~id in
  let ctx = AudioContext.create () in
  let src = ctx##createMediaElementSource audio in
  create_from_src ~ctx ~src ~max_sources
;;

let create_from_mic ~max_sources =
  let open Js_std in
  let constraints =
    MediaStreamConstraints.of_js_expr "{audio : true, video : false}"
  in
  Lwt.wrap (fun c -> get_user_media_exn c ~constraints)
  >>= fun stream ->
  let ctx = AudioContext.create () in
  let src = ctx##createMediaStreamSource stream in
  create_from_src ~ctx ~src ~max_sources |> Lwt.return
;;

let on_event t ~f = t.on_event <- f :: t.on_event
let set_debug t ctx = t.debug <- ctx
