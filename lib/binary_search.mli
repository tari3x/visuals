
module type S = sig
  type t

  val search
    :  start:(t * t)
    -> test:(t -> int)
    -> t
end

module Float : S with type t = float
module Int   : S with type t = int
