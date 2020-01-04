(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

;;

let () =
  top_level (fun () ->
    Gui_client.main Local_config.config (Box.create ~kind:Spark.Ctl.spot ()))

