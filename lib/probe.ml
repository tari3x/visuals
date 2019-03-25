open Core

module Probe = struct
  type t =
    { mutable started : Time_ns.t option
    ; mutable finished : Time_ns.Span.t list
    } [@@deriving sexp]

  let create () =
    { started = None
    ; finished = []
    }

  module Summary = struct
    type t =
      { average : Time_ns.Span.t
      ; total : Time_ns.Span.t
      } [@@deriving sexp]
  end

  let summary t =
    let n = List.length t.finished in
    let total =
      if n = 0 then Time_ns.Span.zero
      else List.reduce_exn t.finished ~f:Time_ns.Span.(+)
    in
    let average = Time_ns.Span.scale total Float.(1. / float n) in
    { Summary. average; total }
end

type t = Probe.t String.Table.t [@@deriving sexp]

let (t : t) = String.Table.create ()

module Summary = struct
  type t = Probe.Summary.t String.Table.t [@@deriving sexp]
end

let summary () =
  Hashtbl.map t ~f:Probe.summary

let start name =
  let probe = Hashtbl.find_or_add t name ~default:Probe.create in
  match probe.started with
  | None -> probe.started <- Some (Time_ns.now ())
  | Some _ -> failwithf !"%{Sexp}" [%message "double start" name] ()

let stop name =
  let probe = Hashtbl.find_exn t name in
  match probe.started with
  | None -> failwithf !"%{Sexp}" [%message "stop without start" name] ()
  | Some time ->
    let span = Time_ns.(diff (now ()) time) in
    probe.finished <- span :: probe.finished;
    probe.started <- None

let with_probe name f =
  start name;
  let result = f () in
  stop name;
  result

let print_and_clear () =
  print_s [%message (summary () : Summary.t)];
  Hashtbl.clear t

