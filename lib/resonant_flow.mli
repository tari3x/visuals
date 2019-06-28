(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common

(* CR: not accurate *)
(* Returns a value that varies like distance to a moving point on a circle, from
   a position off centre. Randomizes period length and the position after each
   period. *)

type t

val create_exn
  :  period_range : (Time.Span.t * Time.Span.t)
  (* max value will be reached in each period, min value is just a bound. *)
  -> value_range  : (float * float)
  -> t

val eval : t -> float
