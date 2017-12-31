(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

(* CR-someday: MFCC? JS-Xtract can do it. *)

open Base
open Lwt
open Common
open Js_std
open Dom_wrappers
open Web_audio
open Typed_array
open Geometry

(* CR-someday: how much does zero-allocation matter here? *)

let beat_lockout = Time.Span.of_seconds 0.1
let threshold_decay_rate = 0.3

(*
let volume_delta_ewma_half_life = 10.
let volume_delta_ewma_decay =
  let t = volume_delta_ewma_half_life in
  let beta = exp (neg (log 2. / t)) in
  let alpha = 1. - beta in
  assert (alpha > 0.);
  assert (alpha < 1.);
  alpha
*)

type t =
  { analyser : AnalyserNode.t
  ; mutable bins : uint8Array Js.t
  ; mutable prev_bins : uint8Array Js.t
  ; mutable volume : float
  ; mutable prev_volume : float
(*
  ; mutable volume_delta_ewma : float
*)
  ; mutable max_volume_delta : float
  ; mutable last_beat_time : Time.t
  ; mutable last_beat_volume : float
  ; mutable on_beat : (unit -> unit) list
  ; mutable debug : Ctx.t option
  }

let num_bins t =
  t.bins##.length

let bin_volume_exn t i =
  float (Typed_array.unsafe_get t.bins i) /. 256.

let prev_bin_volume_exn t i =
  float (Typed_array.unsafe_get t.prev_bins i) /. 256.

let draw t ctx =
  let open Float in
  Ctx.clear ctx;
  Ctx.save ctx;
  let ctx_w = Ctx.width ctx in
  let ctx_h = Ctx.height ctx in
  let volume_w = 100. in
  let empty_w = 100. in
  let bin_w = (ctx_w - volume_w - empty_w) / float (num_bins t) in
  let cursor = ref 0. in
  let draw_bar ~value ~width =
    let height = value * ctx_h in
    let v = Vector.create_float !cursor (ctx_h - height) in
    Ctx.fill_rect ctx v ~width ~height;
    cursor := !cursor + width
  in
  let volume_delta = t.volume - t.prev_volume in
  t.max_volume_delta <-
    max t.max_volume_delta (abs volume_delta);
  (*
  let volume_delta_ewma =
    let a = volume_delta_ewma_half_life in
    a * volume_delta + (1. - a) * t.volume_delta_ewma
  in
  t.volume_delta_ewma <- volume_delta_ewma;
  *)
  let bin_deltas =
    Sequence.init (num_bins t) ~f:(fun i ->
      bin_volume_exn t i - prev_bin_volume_exn t i)
  in
  let max_delta =
    Sequence.max_elt_exn bin_deltas ~cmp:(fun x1 x2 -> compare (abs x1) (abs x2))
    |> abs
  in
  Sequence.iteri bin_deltas ~f:(fun _i delta ->
    let delta =
      if delta = 0. || t.max_volume_delta = 0.
      then delta
      else delta * volume_delta / (max_delta * t.max_volume_delta)
    in
    let arg = (delta + 1.) / 2. in
    let color =
      Color.interpolate ~arg
        [ Color.blue; Color.white; Color.red ]
    in
    (* let value = bin_volume_exn t i in *)
    Ctx.set_fill_color ctx color;
    draw_bar ~value:1. ~width:bin_w
  );
  draw_bar ~value:0. ~width:empty_w;
  draw_bar ~value:t.volume ~width:volume_w;
  Ctx.restore ctx

let draw t =
  Option.iter t.debug ~f:(draw t)

let rec update_loop (t : t) =
  let open Float in
  (*
  Lwt_js_events.request_animation_frame ()
  >>= fun () ->
  *)
  Lwt_js.sleep 0.01
  >>= fun () ->
  let bins = t.prev_bins in
  t.prev_bins <- t.bins;
  t.bins <- bins;
  t.prev_volume <- t.volume;
  t.analyser##getByteFrequencyData t.bins;
  let last_bin = num_bins t in
  let volume =
    t.bins##subarray 0 last_bin
    |> Typed_array.fold ~init:0 ~f:Int.(+)
    |> float
    |> Float.scale (1. /. float last_bin)
    |> Float.scale (1. /. 256.)
  in
  t.volume <- volume;
  let now_ = Time.now () in
  let last_beat_age = Time.(now_ - t.last_beat_time) in
  let threshold =
    t.last_beat_volume
    - (Time.Span.to_seconds last_beat_age) * threshold_decay_rate
    |> (Float.max 0.0)
  in
  if Float.(volume > threshold + 0.05)
    && Time.Span.(last_beat_age > beat_lockout)
  then begin
    List.iter t.on_beat ~f:(fun f -> f ());
    t.last_beat_time <- now_;
    t.last_beat_volume <- volume
  end;
  draw t;
  update_loop t

let create_from_src ~ctx ~src =
  let analyser = ctx##createAnalyser () in
  analyser##.smoothingTimeConstant := 0.;
  src##connect ~destination:(analyser :> AudioNode.t);
  analyser##connect ~destination:(ctx##.destination :> AudioNode.t);
  (* 1024 by default *)
  let num_bins = analyser##.frequencyBinCount in
  let bins = new%js uint8Array num_bins in
  let prev_bins = new%js uint8Array num_bins in
  let t =
    { analyser
    ; bins
    ; prev_bins
    ; volume = 0.
    ; prev_volume = 0.
    ; max_volume_delta = 0.
    ; last_beat_time = Time.now ()
    ; last_beat_volume = 1.
    ; on_beat = []
    ; debug = None
    }
  in
  Lwt.async (fun () -> update_loop t);
  t

let create_from_html ~id =
  let audio = Audio.create ~id in
  let ctx = AudioContext.create () in
  let src = ctx##createMediaElementSource audio in
  create_from_src ~ctx ~src

let create_from_mic () =
  let constraints =
    MediaStreamConstraints.of_js_expr "{audio : true, video : false}"
  in
  Lwt.wrap (fun c -> get_user_media_exn c ~constraints)
  >>= fun stream ->
  let ctx = AudioContext.create () in
  let src = ctx##createMediaStreamSource stream in
  create_from_src ~ctx ~src
  |> Lwt.return

let on_beat t ~f =
  t.on_beat <- f :: t.on_beat

let set_debug t ctx =
  t.debug <- ctx
