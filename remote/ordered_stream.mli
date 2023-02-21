(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)


(* If messages [A] and [B] are created using the same [t] and [A] is created
   before [B] then [A] will be read before [B]. This holds even if [A] and [B]
   are serialized between being created and read, so one can use it to impose
   ordering on an unordered network stream. *)

module Element : sig
  type 'a t
end

type 'a t

val create : max_buffer_size:int -> 'a t

val create_element : 'a t -> 'a -> 'a Element.t

val write : 'a t -> 'a Element.t -> unit

val reader : 'a t -> 'a Lwt_stream.t
