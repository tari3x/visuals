(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

module type S = sig
  type t

  val search
    :  bounds:(t * t)
    -> test:(t -> int)
    -> ?start:t
    -> unit
    -> t
end

module Float : S with type t = float
module Int   : S with type t = int
