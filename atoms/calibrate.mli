
    
(* If you replot the markers they map back to themselves in the
   same order. *)
val calibrate
  :  Video.t
  -> Context.t
  -> Math.Matrix.t Lwt.t
