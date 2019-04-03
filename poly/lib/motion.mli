(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Async

val smooth_speed
  :  point:(float -> 'a)
  -> distance:('a -> 'a -> float)
  (* CR: make it max speed, normailize per framerate.  *)
  -> max_distance:float
  -> desired_step_size:float
  -> 'a Pipe.Reader.t
