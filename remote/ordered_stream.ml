(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
open Std_internal

module Id = Id(struct let name = "Ordered_stream_id" end)

module Element = struct
  type 'a t =
    { id: Id.t
    ; seq : int
    ; value : 'a
    }

  let create ~id ~seq ~value =
    { id; seq; value }

  let id t = t.id
  let seq t = t.seq
  let value t = t.value

  let to_string { id; seq; value = _ } =
    sprintf "{ id = %s; seq = %d }"
      (Id.to_string id)
      seq
end

module Buffer = struct
  type 'a t =
    { mutable buffer : 'a Element.t list
    ; mutable buffer_size : int
    ; mutable next_receive : int option
    ; max_buffer_size : int
    ; write : ('a option -> unit)
    }

  let create ~max_buffer_size ~write =
    { buffer = []
    ; buffer_size = 0
    ; next_receive = None
    ; max_buffer_size
    ; write
    }

  let _to_string { buffer; next_receive; buffer_size = _
                 ; max_buffer_size = _; write = _ } =
    let buffer =
      List.map buffer ~f:Element.to_string
      |> String.concat ~sep:"; "
    in
    let next_receive = Option.to_string Int.to_string next_receive in
    sprintf "{ buffer = [ %s ]; next_receive = %s }"
      buffer next_receive

  let rec insert t x xs =
    let seq = Element.seq x in
    match xs with
    | [] -> [ x ]
    | x' :: xs ->
      let seq' = Element.seq x' in
      if seq < seq' then x :: x' :: xs
      else begin
        debug "messages out of order, buffer size: %d" t.buffer_size;
        x' :: insert t x xs
      end

  let rec flush t = function
    | [] -> []
    | x :: xs ->
      let seq = Element.seq x in
      let next_receive =
        match t.next_receive with
        | Some seq -> seq
        | None ->
          t.next_receive <- Some seq;
          seq
      in
      (* Drop messages that are earlier than the first message we received for
         this id. *)
      if seq < next_receive then flush t xs
      else if seq > next_receive then x :: xs
      else begin
        t.write (Some (Element.value x));
        t.buffer_size <- t.buffer_size - 1;
        t.next_receive <- Some (next_receive + 1);
        flush t xs;
      end

  (* CR: why does the first message we receive on the init channel have id 1? *)
  let write t x =
    t.buffer <- insert t x t.buffer;
    t.buffer_size <- t.buffer_size + 1;
    t.buffer <- flush t t.buffer;
    if t.buffer_size > t.max_buffer_size
    then failwithf "max buffer size %d exceeded" t.max_buffer_size ()
end

type 'a t =
  { id : Id.t
  ; buffers : 'a Buffer.t Hashtbl.M(Id).t
  ; mutable next_send : int
  ; reader : 'a Lwt_stream.t
  ; write : ('a option -> unit)
  ; max_buffer_size : int
  }

let create ~max_buffer_size =
  let id = Id.create () in
  let reader, write = Lwt_stream.create () in
  { id
  ; buffers = Hashtbl.create (module Id)
  ; next_send = 0
  ; reader
  ; write
  ; max_buffer_size
  }

let create_element t value =
  let seq = t.next_send in
  t.next_send <- seq + 1;
  Element.create ~id:t.id ~seq ~value

let reader t =
  t.reader

let write t x =
  let id = Element.id x in
  let buffer = Hashtbl.find_or_add t.buffers id ~default:(fun () ->
    Buffer.create ~max_buffer_size:t.max_buffer_size ~write:t.write)
  in
  Buffer.write buffer x
