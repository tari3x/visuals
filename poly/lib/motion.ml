(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Async
open Common

let _debug a = debug_s ~enabled:true a

let smooth_speed
    ~point:create_point
    ~distance
    ~max_distance
    ~desired_step_size =
  let open Float in
  let reader, writer = Pipe.create () in
  let move param point step =
    let param = min 1. (param + step) in
    let new_point = create_point param in
    let distance = distance point new_point in
    param, new_point, distance
  in
  let next param point =
    let rec loop ~step =
      let param, point, distance = move param point step in
      if distance <= max_distance
      then param, point
      else begin
        let step = step * 0.9 in
        loop ~step
      end
    in
    loop ~step:desired_step_size
  in
  let rec loop param point =
    let%bind () = Pipe.write writer point in
    if param >= 1. then return ()
    else begin
      let param, point = next param point in
      loop param point
    end
  in
  don't_wait_for begin
    let%bind () = loop 0. (create_point 0.) in
    Pipe.close writer;
    return ()
  end;
  reader
