open Js

module Matrix : sig
  type t = float js_array Js.t js_array Js.t
end

val multiply : Matrix.t -> Matrix.t -> Matrix.t

val inv : Matrix.t -> Matrix.t
