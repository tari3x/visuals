(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Core
module Q = Queue

module Sample = struct
  type t = { param : float; value : float } [@@deriving sexp_of]
end

type t = { q : Sample.t Q.t; mutable avg : float; window : float }

let create ~window =
  let q = Q.create () in
  { q; avg = 0.; window }

let cleanup t ~param =
  let open Float in
  let rec loop () =
    match Q.peek t.q with
    | None -> ()
    | Some sample ->
        if sample.param + t.window < param then (
          let n = Q.length t.q |> Float.of_int in
          if n = 1. then t.avg <- 0.
          else t.avg <- ((t.avg * n) - sample.value) / (n - 1.);
          ignore (Q.dequeue t.q : Sample.t option);
          loop ())
  in
  loop ()

let add t ~param ~value =
  let open Float in
  cleanup t ~param;
  let sample = { Sample.param; value } in
  let n = Q.length t.q |> Float.of_int in
  t.avg <- ((n * t.avg) + value) / (n + 1.);
  Q.enqueue t.q sample

let get t = if Q.is_empty t.q then None else Some t.avg
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
  [%expect {| (value (0.5)) |}];
  add 2. 2.;
  [%expect {| (value (1)) |}];
  add 3. 3.;
  [%expect {| (value (2)) |}];
  add 4. 2.;
  [%expect {| (value (2.3333333333333335)) |}];
  add 5. 1.;
  [%expect {| (value (2)) |}];
  add 6. 0.;
  [%expect {| (value (1)) |}]
