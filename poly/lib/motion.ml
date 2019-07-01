(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Async
open Common

let debug a = debug_s ~enabled:false a

let smooth_speed
    ~point:create_point
    ~distance
    ~max_distance
    ~desired_step_size =
  let open Float in
  let reader, writer = Pipe.create () in
  let min_distance = 0.5 * max_distance in
  let move ~param point ~step =
    let param = min 1. (param + step) in
    let new_point = create_point param in
    let distance = distance point new_point in
    param, new_point, distance
  in
  let step param point ~step_prior =
    debug [%message "starting search" (step_prior : float)];
    let test step =
      debug [%message (step : float)];
      let _, _, distance = move ~param point ~step in
      if      distance > max_distance then 1
      else if distance < min_distance then -1
      else 0
    in
    Bisection.Float.search
      ~bounds:(0., desired_step_size)
      ~test
      ~start:step_prior
      ()
  in
  let rec loop param point ~step_prior =
    if Pipe.is_closed writer then return ()
    else begin
      let%bind () = Pipe.write writer point in
      if param >= 1. then begin
        Pipe.close writer;
        return ()
      end
      else begin
        Probe.start "motion";
        let step = step param point ~step_prior in
      (* CR-someday: we compute the last point twice. *)
        let param, point, _ = move ~param point ~step in
        Probe.stop "motion";
        loop param point ~step_prior:step
      end
    end
  in
  don't_wait_for (loop 0. (create_point 0.) ~step_prior:desired_step_size);
  reader
