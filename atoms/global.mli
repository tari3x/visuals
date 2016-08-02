open Common

type t

val create
  :  viewport_width:float
  -> viewport_height:float
  -> is_server:bool
  (* CR: fold this into [is_server] *)
  -> max_clients:int
  -> t Lwt.t

val iter : t -> f:(Shape.t -> unit) -> unit
val find : t -> f:(Shape.t -> bool) -> Shape_id.t option

val get     : t -> Shape_id.t -> Shape.t option
val get_exn : t -> Shape_id.t -> Shape.t

val request : t -> Shape_id.t -> unit
val release : t -> Shape_id.t -> unit

val add : t -> Shape.t -> Shape_id.t
val change : t -> Shape_id.t -> f:(Shape.t -> Shape.t) -> unit
val delete : t -> Shape_id.t -> unit

val on_change : t -> f:(Shape_id.t -> Shape.t -> unit) -> unit

val now_on_server : t -> Time.t
