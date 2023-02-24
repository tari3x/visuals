(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Core_kernel
module Q = Queue
module H = Pairing_heap

module Sample = struct
  type t =
    { param : float
    ; value : float
    }
  [@@deriving sexp_of]

  let compare t1 t2 = Float.compare t1.value t2.value
end

type t =
  { h : Sample.t H.t
  ; q : Sample.t H.Elt.t Q.t
  ; window : float
  }
[@@deriving sexp_of]

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

let get t =
  match H.top t.h with
  | None -> None
  | Some sample -> Some sample.value
;;

let get_exn t = get t |> Option.value_exn

let%expect_test _ =
  let t = create ~window:2. in
  let print () =
    let value = get t in
    print_s [%message (value : float option)]
  in
  let add param value =
    add t ~param ~value;
    print ()
  in
  print ();
  [%expect {| (value ()) |}];
  add 0. 0.;
  [%expect {| (value (0)) |}];
  add 1. 1.;
  [%expect {| (value (0)) |}];
  add 2. 2.;
  [%expect {| (value (0)) |}];
  add 3. 2.;
  [%expect {| (value (1)) |}];
  add 4. 2.;
  [%expect {| (value (2)) |}];
  add 5. 1.;
  [%expect {| (value (1)) |}]
;;
