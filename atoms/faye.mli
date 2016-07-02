open Common

module Channel : sig
  type t
  val global : t
  val create : unit -> t
end

module Message : sig
  module Init : sig
    type t =
      { shapes : (Shape_id.t * Shape.t * Client_id.t option) list
      ; width : float
      ; height : float
      ; time : float
      }
  end

  type t =
  | Request of (Client_id.t * Shape_id.t)
  | Grant   of (Client_id.t * Shape_id.t)
  | Release of Shape_id.t
  | Create  of (Shape_id.t * Client_id.t * Shape.t)
  | Set     of (Shape_id.t * Shape.t)
  | Delete  of Shape_id.t
  | Request_init of Channel.t
  | Init    of Init.t

  val to_string : t -> string
end

type t

val create : unit -> t

val publish : t -> Channel.t -> Message.t -> unit

(* Clients will not receive their own messages. *)
val subscribe : t -> Channel.t -> f:(Message.t -> unit) -> unit
