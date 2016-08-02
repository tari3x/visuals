
module Channel : sig
  type t
  val global : t
  val create : unit -> t
end

(* CR: guarantee the order of messages per channel and client. *)
type 'a t

val create : to_string:('a -> string) -> 'a t

val publish : 'a t -> Channel.t -> 'a -> unit

(* Clients will receive their own messages. *)
val subscribe_with_try : 'a t -> Channel.t -> f:('a -> unit) -> unit
