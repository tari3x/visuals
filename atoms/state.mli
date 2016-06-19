open Common

type t

val create : is_leader:bool -> t

val canvas : t -> Html.canvasElement Js.t

val process_action : t -> Action.t -> unit
