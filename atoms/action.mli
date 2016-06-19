open Common

module Button_action : sig
  type t = [ `down | `up | `move ]
end

type t =
| Move of Point.t
| Down of Button.t
| Up   of Button.t

val of_mouse_event
  :  Html.mouseEvent Js.t
  -> Button_action.t
  -> t

val of_touch_event
  :  Html.touchEvent Js.t
  -> Button_action.t
  -> t
