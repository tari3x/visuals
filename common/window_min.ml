(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
module Q = Queue
module H = Pairing_heap

module Sample = struct
  type t =
    { param : float
    ; value : float
    }

  let compare t1 t2 = Float.compare t1.value t2.value
end

type t =
  { h : Sample.t H.t
  ; q : Sample.t H.Elt.t Q.t
  ; window : float
  }

let create ~window =
  let h = H.create ~cmp:Sample.compare () in
  let q = Q.create () in
  { h; q; window }
;;

let cleanup t ~param =
  let rec loop () =
    match Q.peek t.q with
    | None -> ()
    | Some elt ->
      let sample = H.Elt.value_exn elt in
      if Float.(sample.param + t.window < param)
      then (
        ignore (Q.dequeue t.q : Sample.t H.Elt.t option);
        H.remove t.h elt;
        loop ())
  in
  loop ()
;;

let add t ~param ~value =
  cleanup t ~param;
  let sample = { Sample.param; value } in
  let elt = H.add_removable t.h sample in
  Q.enqueue t.q elt
;;

let min t =
  match H.top t.h with
  | None -> None
  | Some sample -> Some sample.value
;;
