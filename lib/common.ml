(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Lwt
open Js

include Printf

module Html = Dom_html

(* CR-someday: unify catching this *)
exception Shutdown

(* [Base] doesn't have [Int.Replace_polymorphic_compare] *)
include struct
  open Int
  let (=) = (=)
  let max = max
  let min = min
  let (<) = (<)
  let (>) = (>)
  let (>=) = (>=)
  let (<=) = (<=)
end

let debug_table x : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.table")
    [| Js.Unsafe.inject x |]
let debug f = Printf.ksprintf (fun s -> Firebug.console##(log (Js.string s))) f
let alert f = Printf.ksprintf (fun s -> Html.window##(alert (Js.string s))) f
let console_error f =
  Printf.ksprintf (fun s -> Firebug.console##(error (Js.string s))) f

(* CR-someday: do something about dev vs prod. *)
let error = console_error

(* Looks like this is no longer necessary? A top level
   [Lwt.async (fun () -> failwith "FAILURE");]
   gets caught all right.
*)
(*
let () =
  Lwt.async_exception_hook := fun exn ->
    error "uncaught exn in aysnc: %s" (Printexc.to_string exn)
*)

let raise_s s =
  new%js Js.error_constr (string s)
  |> Js.raise_js_error

let raise e =
  raise_s (Exn.to_string e)

let failwithf f =
  Printf.ksprintf raise_s f

let current_url =
  sprintf "http://%s" (Js.to_string (Html.window##.location##.host))

let float = Float.of_int
let int   = Int.of_float

let pi =
  let pi = Float.acos (- 1.0) in
  assert (Float.(>=) pi 3.0 && Float.(<=) pi 4.);
  pi

module Float = struct
  include Float
  let infty = 10000000000000000000000000.
end

module Optdef = struct
  include Optdef

  let value_exn t =
    get t (fun () -> failwith "Optdef: undefined")
end

module Opt = struct
  include Opt

  let value_exn ~here ?(message = "") t =
    Opt.get t (fun () ->
      failwithf !"Opt.value_exn: %{Source_code_position}: %s" here message ())
end

module Option = struct
  include Option

  let to_string a_to_string = function
    | None -> "none"
    | Some a -> a_to_string a

  let some_if b x =
    if b then Some x else None
end

module Sequence = struct
  include Sequence

  let max_elt_exn xs ~cmp =
    max_elt xs ~cmp |> Option.value_exn
end

module List = struct
  include List

  let delete xs x ~equal =
    filter xs ~f:(fun x' -> not (equal x' x))

  let bring_to_front xs x ~equal =
    x :: delete xs x ~equal

  let diff xs ys ~equal =
    filter xs ~f:(fun x ->
      not (List.mem ys x ~equal))

  let max_elt_exn xs ~cmp =
    max_elt xs ~cmp |> Option.value_exn
end

module Typed_array = struct
  include Typed_array

  let fold (t : ('a, 'b) typedArray Js.t) ~init ~f =
    let rec loop i acc =
      match get t i |> Optdef.to_option with
      | None -> acc
      | Some x -> loop (i + 1) (f acc x)
    in
    loop 0 init

  let iter (t : ('a, 'b) typedArray Js.t) ~f =
    let rec loop i =
      match get t i |> Optdef.to_option with
      | None -> ()
      | Some x ->
        f x;
        loop (i + 1)
    in
    loop 0
end

(* CR-someday: There is no way to close the reader. Try [create_bounded] or
   [Lwt_pipe].*)
module Lwt_stream = struct
  include Lwt_stream

  let find t ~f =
    find f t
    >>= fun x ->
    return (Option.value_exn x)

  let iter_with_try t ~f =
    let f x =
      try f x
      with e -> begin error "%s" (Exn.to_string e); () end
    in
    iter f t

  let filter_map t ~f =
    filter_map f t

  let take t ~n =
    let rec loop n acc =
      if n = 0 then Lwt.return (List.rev acc)
      else begin
        next t
        >>= fun x ->
        loop (n - 1) (x :: acc)
      end
    in
    loop n []
end

module type Id = sig
  include Identifiable.S
  val create : unit -> t
  val to_string : t -> string
end

module Id(M: sig val name : string end) : Id = struct
  include String

  let () =
    Random.self_init ()

  let create () =
    Printf.sprintf "%s%d"
      M.name
      (Random.int 100_000_000)
end

module Client_id = Id(struct let name = "Client_id" end)
module Box_id    = Id(struct let name = "Box_id" end)

module Fn = struct
  let flip f x y =
    f y x

  let const c _ = c
end

(* [Base] doesn't contain [Time] it seems. *)
module Time : sig
  type t [@@deriving sexp]

  (* prints the float *)
  val to_string : t -> string

  val of_sec : float -> t
  val to_sec : t -> float
  val now : unit -> t

  module Span : sig
    type t [@@deriving sexp]
    val zero : t
    val to_sec : t -> float
    val of_sec : float -> t
    val (>) : t -> t -> bool
    val (<) : t -> t -> bool
    val ( * ) : t -> float -> t
  end

  val (-) : t -> t -> Span.t
  val (+) : t -> Span.t -> t

  val sub : t -> Span.t -> t
end = struct
  type t = float [@@deriving sexp]

  let to_string = Float.to_string

  module Span = struct
    include Float
    let to_sec t = t
    let of_sec t = t
  end

  let now () = Unix.gettimeofday ()
  let of_sec t = t
  let to_sec t = t

  let (-) = (-.)
  let (+) = (+.)

  let sub = (-.)
end

module Lwt = struct
  include Lwt

  let wrap f =
    let (t, w) = Lwt.task () in
    let cont x = Lwt.wakeup w x in
    f cont;
    t

  let every ~span ~f =
    let span = Time.Span.to_sec span in
    let rec loop () =
      f ();
      Lwt_js.sleep span
      >>= fun () ->
      loop ()
    in
    Lwt.async loop

  module Let_syntax = struct
    module Let_syntax = struct
      let return = return
      let bind t ~f = bind t f
      let map t ~f = map t f
      let both t1 t2 =
        t1 >>= fun t1 ->
        t2 >>= fun t2 ->
        return (t1, t2)
    end
  end
end

(* CR: move this to [Dom_wrappers]. *)
let add_event_listener elt event ~f =
  Html.addEventListener elt event
    (Html.handler
       (fun ev ->
         begin
           try f ev with
           | Shutdown -> raise Shutdown
           | exn ->
             error "uncaught exn in handler: %s"
               (Exn.to_string exn)
         end;
         Js._true))
    Js._true
  |> ignore

let top_level f =
  add_event_listener Html.window Html.Event.load ~f:(fun _ ->
    Lwt.async (fun () -> Lwt.catch f raise))

let get_element_by_id id coerce_to =
  (Opt.bind (Html.document##getElementById (string id)) coerce_to)
  |> Opt.value_exn ~here:[%here] ~message:(sprintf "can't find element %s" id)
