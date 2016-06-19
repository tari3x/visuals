open Common

module Button_action = struct
  type t = [ `down | `up | `move ]
end

type t =
| Move of Point.t
| Down of Button.t
| Up   of Button.t

let of_mouse_event ev = function
  | `down -> Down (Mouse_event.button ev)
  | `up   -> Up   (Mouse_event.button ev)
  | `move -> Move (Mouse_event.client_coords ev)

let of_touch_event ev = function
  | `down -> Down `touch
  | `up   -> Up   `touch
  | `move -> Move (Touch_event.client_coords ev)
