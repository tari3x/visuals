(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Common

type 'a t

val create
  :  viewport_width:float
  -> viewport_height:float
  -> is_server:bool
  (* CR: fold this into [is_server] *)
  -> max_clients:int
  -> sexp_of_a:('a -> Sexp.t)
  -> 'a t Lwt.t

val iter : 'a t -> f:('a Box.t -> unit) -> unit
val find : 'a t -> f:('a Box.t -> bool) -> Box_id.t option

val get     : 'a t -> Box_id.t -> 'a Box.t option
val get_exn : 'a t -> Box_id.t -> 'a Box.t

val request : _ t -> Box_id.t -> unit
val release : _ t -> Box_id.t -> unit

val add : 'a t -> 'a Box.t -> Box_id.t
val change : 'a t -> Box_id.t -> f:('a Box.t -> 'a Box.t) -> unit
val delete : _  t -> Box_id.t -> unit

val on_change : 'a t -> f:(Box_id.t -> 'a Box.t -> unit) -> unit

val now_on_server : _ t -> Time.t
